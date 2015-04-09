/*
 * Copyright (c) <2014>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */

package ch.usi.inf.l3.oa.plugin


import ch.usi.inf.l3._
import piuma.neve.NeveDSL._
import oa.library._
import oa.quals._




@plugin(DefaultMethodsComponent,
        SubstituteCallersComponent) class ObjectAlgebraPlugin {
  val beforeFinder = "typer"
  val name: String = "object-algebra"
  describe("""A compiler plugin to add default objects for lifter and empty""")
  var verbose = false

  def basePkg = "ch.usi.inf.l3.oa"
  def lifterAnno = s"${basePkg}.quals.lifter"
  def mergerAnno = s"${basePkg}.quals.merger"
  def emptyAnno = s"${basePkg}.quals.empty"
  def lifterName = s"${basePkg}.library.TLifter"

  def debug(txt: => String): Unit = {
    if(verbose)
      println(txt)
  }

  override def processOptions(options: List[String], error: String => Unit) = {
    for (option <- options) {
      if (option == "verbose" || option == "v")
        verbose = true
      else 
        error(s"Option not understood: ${option}")
    }
  }
}


@treeTransformer("defaultMethods") class DefaultMethodsComponent {
  rightAfter("classFinder")
  
  plugin ObjectAlgebraPlugin

  private val lifter = getClassByName(lifterName)

  def isIn(x: Symbol, members: List[Tree]): Boolean = {
    members.foldLeft(false)((z, v) => {
      z || {
        (x.name == v.symbol.name) && ((x.tpe, v.symbol.tpe) match {
          case (MethodType(ps1, _), MethodType(ps2, _)) =>
            (ps1.zip(ps2)).foldLeft(true)((z, y) => {
              z && !(y._1.info =:= y._2.info)
            })
          case _ => false
        })
      }
    })
  }

  // Default delegate generator
  private def generateDelegates(members: List[Symbol], 
                                field: Symbol,
                                clazzSymbol: Symbol,
                                lifterSymbol: Symbol,
                                implemented: List[Tree]): List[Tree] = {
    members.filter((x) => {
      isMethod(x) && isAbstract(x) && isPublic(x) && !isIn(x, implemented)
    }).map((x) => {
      debug(s"Implementing method ${x}")
      val mthd = clazzSymbol.newMethod(x.name.toTermName, 
        lifterSymbol.pos.focus, Flag.SYNTHETIC)
      val (tpe, params): (Type, List[Ident]) = x.info match {
        case t @ MethodType(params, ret) if !t.takesTypeArgs =>
          val nparams = params.map((y) => {
            mthd.newSyntheticValueParam(y.tpe, y.name.toTermName) 
          })
        (MethodType(nparams, ret), nparams.map(Ident(_)))
        case t @ NullaryMethodType(ret) if !t.takesTypeArgs =>
          (NullaryMethodType(ret), Nil)
        case t =>
          debug("Methods of feature cannot take type parameters")
          throw new Exception("Methods of feature cannot " +
            "take type parameters")
      }

      mthd.setInfoAndEnter(tpe)

      typed { 
        val slct = Select(mkThisSelect(clazzSymbol, field.name.toTermName), 
                          x.name.toTermName)
        val app = tpe match {
          case x: NullaryMethodType =>
            slct
          case _ => Apply(slct, params)
        }
        DefDef(mthd, app)
      }

    })
  }


  def transform(tree: Tree): Tree = {
    val ntree = tree match {
      case module @ ModuleDef(mods, name, template) 
            if hasAnnotation(module, lifterAnno) => 
        debug(s"Entered module ${name}")
        debug(s"${module.symbol} has lifter annotation")
        getAnnotationArguments(module, lifterAnno) match {
          case List(Literal(Constant(f: String)),
                    Literal(Constant(s: String))) =>
            debug(s"The module produces lifter for ${f} and ${s}")
            val fst = getClassByName(f)
            val snd = getClassByName(s)
            require(fst != snd, {
              val msg = "The two arguments of @lifter must be different"
              debug(msg)
              msg
            })
            debug(s"${fst} is the first lifted feature")
            debug(s"${snd} is the second lifted feature")
            debug(s"${lifter} is the Lifter class")

            // Implement the default lifter
            val msymb = module.symbol.moduleClass

            val parent = appliedType(lifter, List(fst.toType, snd.toType))
            debug(s"Adding ${parent} to the parent list of ${module.symbol}")
            val lifterObject = module.addParent(parent)

            // The lift method
            val liftMethod = lifter.info.member(TermName("lift"))

            // The features that we want to have lifter for
            val (v1, v2): (Symbol, Symbol) = liftMethod.paramss.flatten match {
              case List(p1, p2) => (p1, p2)
              case _ => 
                // Not likely it will ever fail, but still
                val msg = "Method lifter should take exactly two parameters"
                debug(msg)
                throw new Exception(msg)
            }

            // parents of the compound type
            val parents = List(definitions.AnyRefClass.toType,
                                         fst.toType, snd.toType)
            def compoundType = RefinedType(parents, newScope)
            debug(s"The compound type of ${lifterObject.symbol} is ${compoundType}")


            // TOOD: Can we have ``implement with this signature'' in Piuma?
            val (methodSymbol, p1, p2) = {
              debug(s"Implementing method ${liftMethod}")
              val mthd = msymb.newMethod(liftMethod.name.toTermName, 
                msymb.pos.focus, Flag.SYNTHETIC)
              debug(s"Adapting the first type parameter to ${fst} and " +
                    s"the second to ${snd}")
              val (tpe, p1, p2): (Type, Symbol, Symbol)= 
                liftMethod.info match {
                  case MethodType(params, ret) =>
                    val nret = compoundType
                    val p1 = mthd.newSyntheticValueParam(fst.toType, 
                                    v1.name.toTermName)
                    val p2 = mthd.newSyntheticValueParam(snd.toType, 
                                    v2.name.toTermName)
                    val nparams = List(p1, p2)
                    (MethodType(nparams, nret), p1, p2)
                  case _ =>
                    val msg = 
                      s"${liftMethod} surprisingly does not have method type"
                    debug(msg)
                    throw new Exception(msg)
                }
              mthd.setInfoAndEnter(tpe)
              (mthd, p1, p2)
            }

            val clazzSymbol =
              msymb.newClassSymbol(TypeName("CompoundFeatures"), 
                msymb.pos.focus)
            val tpe = ClassInfoType(parents, newScope, clazzSymbol)
            clazzSymbol.setInfoAndEnter(tpe)



            val clazzTree: ClassDef = {
              // An empty ClassDef, we augment it in the following statements

              val temp = ClassDef(Modifiers(Flag.SYNTHETIC), 
                clazzSymbol.name.toTypeName,
                Nil,
                Template(parents.map((x) => typed{TypeTree(x)}),
                  noSelfType, Nil)).setSymbol(clazzSymbol)

              // A constructor with two parameters representing the features
              val cnstr1 = mkConstructor(temp, Nil).addSuperConstructorCall(Nil, Nil)
              val f2 = mkConstructorParam(cnstr1, "f2", p2.toType, true)
              val f1 = mkConstructorParam(cnstr1, "f1", p1.toType, true)

              val cnstr2 = addParam(f2, cnstr1)
              val cnstr = addParam(f1, cnstr2)



              // Delegate fst methods to the first feature
              val fmembers = generateDelegates(fst.info.members.toList,
                                               f1.symbol, 
                                               clazzSymbol,
                                               msymb,
                                               Nil)
                
              // Delegate snd methods to the second feature
              val smembers = generateDelegates(snd.info.members.toList,
                                               f2.symbol, 
                                               clazzSymbol,
                                               msymb,
                                               fmembers)

               

              val nbody = (cnstr :: fmembers) ++ smembers


              nbody.foldLeft(temp)((z, y) => {
                z.addMember(y)
              })
            }

            val apply = mkConstructorCall(mkThisSelect(msymb, 
                                          clazzSymbol.name.toTypeName), 
                                        List(Ident(p1), Ident(p2)))
            
            val defdef = typed {
              DefDef(methodSymbol, apply)
            }
            lifterObject.addMember(clazzTree).addMember(defdef)
            
          case _ => 
            debug(s"${tree.symbol} is not a module with OA annotations")
            tree
        }
      case module @ ModuleDef(mods, name, template) 
            if hasAnnotation(module, emptyAnno) => 
        debug(s"Entered module ${name}")
        debug(s"${module.symbol} has empty annotation")
        getAnnotationArguments(module, emptyAnno) match {
          case List(Literal(Constant(arg: String))) =>
            debug(s"The module produces empty algebra for ${arg}")
            val algebra = getClassByName(arg)
            debug(s"The empty algebra is ${algebra}")
                        
            // Add default implementation for all members
            val msymb = module.symbol.moduleClass

            val parent = appliedType(algebra, 
              algebra.typeParams.map((x) => definitions.AnyRefClass.toType))

            debug(s"Adding ${parent} to the parent list of ${module.symbol}")
            val emptyAlgebra = module.addParent(parent)

            val members = algebra.info.members.toList.filter((x) => {
                isMethod(x) && isAbstract(x) && isPublic(x)
              }).map((x) => {
                debug(s"Implementing method ${x}")
                val mthd = msymb.newMethod(x.name.toTermName, 
                            msymb.pos.focus, Flag.SYNTHETIC)
                debug("Adapting the type parameters to AnyRef")
                val tpe = x.info match {
                  case t @ MethodType(params, ret) if isTypeParameter(ret) &&
                                                      !t.takesTypeArgs =>

                    debug("Adapting the type parameters to AnyRef")
                    val nret = definitions.AnyRefClass.toType 
                    
                    val nparams = params.map((y) => {
                      if(y.info.typeSymbol.isTypeParameter) {
                        val ptpe = definitions.AnyRefClass.toType
                        mthd.newSyntheticValueParam(ptpe, 
                          y.name.toTermName) 
                      }
                      else {
                        mthd.newSyntheticValueParam(y.tpe, y.name.toTermName) 
                      }
                    })
                    MethodType(nparams, nret)
                  case t @ NullaryMethodType(ret) if isTypeParameter(ret) &&
                                                      !t.takesTypeArgs =>
                    debug(s"Method ${x.name} has NullaryMethodType")
                    debug("Adapting the type parameters to AnyRef")
                    val nret = definitions.AnyRefClass.toType 
                    NullaryMethodType(nret)
                  case t if t.takesTypeArgs =>
                    debug("Methods of algebra cannot take type parameters")
                    throw new Exception("Methods of algebra cannot " +
                                        "take type parameters")
                  case _ =>
                    debug("The algebra might only have methods " +
                            "with type parameters as return type unimplemented")
                    throw new Exception("The algebra might only have methods " +
                            "with type parameters as return type unimplemented")
                }


                mthd.setInfoAndEnter(tpe)
                
                typed { DefDef(mthd, q"new java.lang.Object()") }
              })
           
            emptyAlgebra.updateBody(template.body ++ members) 
          case t =>
            debug(s"Error getting algebra name from ${t}")
            throw new Exception(s"Error getting algebra name from ${t}")
        }

      case clazz @ ClassDef(mods, name, tparams, impl) if isTrait(clazz) &&
            hasAnnotation(clazz, mergerAnno) => 

        // TODO: We should automate this
        // trait ExpMerge[A, B] extends AlgExp[A with B] {
        //   val lifter: TLifter[A, B]
        //   val alg1: AlgExp[A]
        //   val alg2: AlgExp[B]
        //   
        //   // Members declared in AlgExp
        //   def Lit(x: Int): A with B = lifter.lift(alg1.Lit(x), alg2.Lit(x))
        //   def Add(e1: A with B, e2: A with B): A with B = 
        //     lifter.lift(alg1.Add(e1, e2), alg2.Add(e1, e2))
        // }

        getAnnotationArguments(clazz, mergerAnno) match {
          case List(Literal(Constant(galg: String))) =>
            val symbol = clazz.symbol

            
            // Add two type parameters to the trait
            val (tparam2, sskolem) = mkTypeParam(symbol, "K")
            val (tparam1, fskolem) = mkTypeParam(symbol, "T")

            val tpref1 = fskolem.toType
            val tpref2 = sskolem.toType

            val clazz1 = clazz.addTypeParam(tparam2).addTypeParam(tparam1)


            // Change the parent of the trait, so it inherits the 
            // algebra it represents
            def compoundType = RefinedType(List(tpref1, 
                                                tpref2), 
                                            newScope)
            val exprAlg = getClassByName(galg)



            // Create two abstract fields to represent the two algebras 
            // that need to be merged
            val fstAlg = mkAbstractField(symbol, "alg1", 
                                        appliedType(exprAlg, List(tpref1)))

            val sndAlg = mkAbstractField(symbol, "alg2", 
                                        appliedType(exprAlg, List(tpref2)))


            // Create an abstract field to represent the lifter of the two algebras
            val lifterType = appliedType(lifter.toType, List(tpref1, tpref2))
            val lifterField = mkAbstractField(symbol, "lifter", lifterType)


            val parent = appliedType(exprAlg, List(compoundType))
            debug(s"Adding ${parent} to the parent list of ${symbol}")

            val clazz2 = clazz1.addParent(parent)



            // implement the members of the exprAlg, by delegating everything to
            // the lifter
            val members = exprAlg.info.members.toList.filter((x) => {
                isMethod(x) && isAbstract(x) && isPublic(x)
              }).map((x) => {
                debug(s"Implementing method ${x}")
                val mthd = symbol.newMethod(x.name.toTermName, 
                            symbol.pos.focus, Flag.SYNTHETIC)
                debug("Adapting the type parameters to T with K")
                val tpe = x.info match {
                  case t @ MethodType(params, ret) if isTypeParameter(ret) &&
                                                      !t.takesTypeArgs =>

                    debug("Adapting the type parameters to T with K")
                    val nret = compoundType
                    
                    val nparams = params.map((y) => {
                      if(y.info.typeSymbol.isTypeParameter) {
                        val ptpe = compoundType
                        mthd.newSyntheticValueParam(ptpe, 
                          y.name.toTermName) 
                      }
                      else {
                        mthd.newSyntheticValueParam(y.tpe, y.name.toTermName) 
                      }
                    })
                    MethodType(nparams, nret)
                  case t @ NullaryMethodType(ret) if isTypeParameter(ret) &&
                                                      !t.takesTypeArgs =>
                    debug(s"Method ${x.name} has NullaryMethodType")
                    debug("Adapting the type parameters to T with K")
                    val nret = compoundType
                    NullaryMethodType(nret)
                  case t if t.takesTypeArgs =>
                    debug("Methods of algebra cannot take type parameters")
                    throw new Exception("Methods of algebra cannot " +
                                        "take type parameters")
                  case _ =>
                    debug("The algebra might only have methods " +
                            "with type parameters as return type unimplemented")
                    throw new Exception("The algebra might only have methods " +
                            "with type parameters as return type unimplemented")
                }


                mthd.setInfoAndEnter(tpe)
                
                val thsLifter = typed {
                  mkThisSelect(symbol, lifterField.symbol.name)
                }
                val thsFstAlg = typed {
                  mkThisSelect(symbol, fstAlg.symbol.name)
                }
                val thsSndAlg = typed {
                  mkThisSelect(symbol, sndAlg.symbol.name)
                }

                val slct = Select(thsLifter,
                                    TermName("lift"))

                //   def Add(e1: A with B, e2: A with B): A with B = 
                //     lifter.lift(alg1.Add(e1, e2), alg2.Add(e1, e2))
                val p1 = mkApply(Select(thsFstAlg,
                                    mthd.name),
                                 tpe.params.map(Ident(_)))

                // p1.setSymbol(p1.info.member(mthd.name))

                val p2 = mkApply(Select(thsSndAlg,
                                          mthd.name),
                                  tpe.params.map(Ident(_)))
                // slct.setSymbol(p1.info.member(mthd.name))

                val apply = mkApply(slct, 
                                    List(p1, p2))
                typed { DefDef(mthd, apply) }
              })
            symbol.flags = Flag.TRAIT | Flag.ABSTRACT

            val nbody = fstAlg :: sndAlg :: lifterField :: members
            nbody.foldLeft(clazz2)((z, y) => {
              z.addMember(y)
            })
          case t =>
            debug(s"Error getting algebra name from ${t}")
            throw new Exception(s"Error getting algebra name from ${t}")
        }
      case clazz @ ClassDef(mods, name, tparams, impl) =>
        clazz
      case _ => 
        debug(s"${tree.symbol} is not a module with OA annotations")
        tree
    }
    super.transform(ntree)
  }
}



@treeTransformer("substitueCallers") class SubstituteCallersComponent {
  rightAfter("refchecks")
  
  plugin ObjectAlgebraPlugin

  def transform(tree: Tree): Tree = {
    val ntree = tree match {
      case Apply(fn, List(Literal(Constant(empty: String)))) 
                              if goodSymbol(fn.symbol) &&
                              fn.symbol.name.toString == "useEmpty" => 
        val emptyObject = rootMirror.getModuleByName(TermName(empty))
        typed {Ident(emptyObject)}
      case Apply(fn, List(Literal(Constant(lifter: String)))) 
                              if goodSymbol(fn.symbol) &&
                              fn.symbol.name.toString == "useLifter" => 
        val lifterObject = rootMirror.getModuleByName(TermName(lifter))
        typed {Ident(lifterObject)}
      case ddef @ DefDef(_, _, List(tparam1, tparam2), 
                  List(List(l, f1, f2)), _, 
                  Apply(fn, List(Literal(Constant(merger: String)))))
                              if  goodSymbol(fn.symbol) &&
                              fn.symbol.name.toString == "mergeWith" => 

        
      val (tpe1, tpe2): (Type, Type) = (f1.symbol.info, f2.symbol.info) match {
        case(TypeRef(_, _, List(t1)), TypeRef(_, _, List(t2))) =>
          (t1, t2)
        case (t1, t2) =>
          throw new Exception(
            s"Bad format of types ${showRaw(t1)} and ${showRaw(t2)}")
      }



        val clazz = typed {


          val mergerTrait = getClassByName(merger)


          val symbol = ddef.symbol.newClassSymbol(TypeName("$anon"), 
                                                  ddef.symbol.pos.focus,
                                                  Flag.FINAL)

          val parents = List(definitions.AnyRefClass.toType,
                            appliedType(mergerTrait, List(tpe1, tpe2)))
          val ctpe = ClassInfoType(parents, newScope, symbol)

          symbol.setInfo(ctpe)


          val temp = ClassDef(Modifiers(Flag.FINAL),
                              symbol.name.toTypeName,
                              Nil,
                              Template(
                                List(TypeTree(definitions.AnyRefClass.toType)),
                                noSelfType,
                                Nil)).setSymbol(symbol)



          val f1tpe = mergerTrait.info.member(TermName("alg1")).toType match {
            case NullaryMethodType(TypeRef(_, tconst, _)) =>
              appliedType(tconst, List(tpe1))
            case t => 
              throw new Exception(s"${t} is not a nullary type")
          }


          val f2tpe = mergerTrait.info.member(TermName("alg2")).toType match {
            case NullaryMethodType(TypeRef(_, tconst, _)) =>
              appliedType(tconst, List(tpe2))
            case t => 
              throw new Exception(s"${t} is not a nullary type")
          }


          val lifterTraitTpe = getClassByName(lifterName).toType
          val liftertpe = appliedType(lifterTraitTpe, List(tpe1, tpe2))

          val cnstr3 = 
            mkConstructor(temp, Nil).addSuperConstructorCall(Nil, Nil)

          
          val a2 = mkConstructorParam(cnstr3, "a2", f2tpe, true)
          val a1 = mkConstructorParam(cnstr3, "a1", f1tpe, true)
          val lr = mkConstructorParam(cnstr3, "lr", liftertpe, true)


          val a1sym = symbol.newMethod(TermName("alg1"),
            symbol.pos.focus, Flag.SYNTHETIC)
          a1sym.setInfoAndEnter(NullaryMethodType(f1tpe))

          val a2sym = symbol.newMethod(TermName("alg2"),
            symbol.pos.focus, Flag.SYNTHETIC)
          a2sym.setInfoAndEnter(NullaryMethodType(f2tpe))

          val cnstr2 = addParam(a2, cnstr3)
          val cnstr1 = addParam(a1, cnstr2)
          val cnstr = addParam(lr, cnstr1)

          val lsym = symbol.newMethod(TermName("lifter"),
            symbol.pos.focus, Flag.SYNTHETIC)
          lsym.setInfoAndEnter(NullaryMethodType(liftertpe))

          
          val alg1 = typed {
            DefDef(a1sym, mkThisSelect(symbol, a1.symbol.name))
          }

          val alg2 = typed {
            DefDef(a2sym, mkThisSelect(symbol, a2.symbol.name))
          } 


          val lift = typed {
            DefDef(lsym, mkThisSelect(symbol, lr.symbol.name))
          } 

          List(alg1, alg2, lift, cnstr).foldLeft(temp)((z, y) => {
            z.addMember(y)
          })
        }

        val call = mkConstructorCall(Ident(clazz.symbol), 
            List(Ident(l.symbol), Ident(f1.symbol), Ident(f2.symbol)))

        ddef.updateRHS(mkBlock(List(clazz), call))
      case _ => tree
    }

    super.transform(ntree)
  }
}

