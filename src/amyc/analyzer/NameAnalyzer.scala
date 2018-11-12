package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames: Map[N.Name, List[N.ModuleDef]] = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    val mods: Map[N.Name,Map[N.Name, List[N.ClassOrFunDef]]] =
      modNames.mapValues(l => l.head)
        .mapValues(modDef => modDef.defs)
        .mapValues(l=> l.groupBy(def0 => def0.name));

    mods.foreach { case (modName, defs) => 
      defs.foreach { case (defName, defsWithName) =>
        if(defsWithName.size > 1) {
          fatal(s"Two Definitions named $defName in module $modName", defsWithName.head.position);
        }
      }
    }

    // Step 3: Discover types and add them to symbol table
    val modsSingleDef: Map[N.Name, List[N.ClassOrFunDef]] = mods.mapValues(m => m.values.toList.flatten)

    modsSingleDef.foreach { case(modName, defsInMod) => 
      defsInMod.foreach(df => 
          df match {
            case N.AbstractClassDef(n) => 
              table.addType(modName, n)
            case _ => ()
          })
    };

    // Step 4: Discover type constructors, add them to table
    modsSingleDef.foreach { case (modName, defsInMod) =>
      defsInMod.foreach(df =>
          df match {
            case N.CaseClassDef(n, fields, parentName) => 
              val parent: Identifier = table.getType(modName,parentName) match {
                case Some(id) => id
                case None => 
                  fatal(s"Could not find type $parentName in $modName", df.position)
              }
              table.addConstructor(modName,
                n,
                fields.map(tt => transformType(tt, modName)),
                parent)
            case _ => ()
          })
    };

    // Step 5: Discover functions signatures, add them to table
    modsSingleDef.foreach { case (modName, defsInMod) =>
      defsInMod.foreach(df =>
          df match {
            case N.FunDef(name, params, retType, body) =>
              table.addFunction(
                modName,
                name,
                params.map(p => transformType(p.tt, modName)),
                transformType(retType, modName))
            case _ => ()
          })
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case N.AbstractClassDef(name) =>
        val Some(id) = table.getType(module, name)
        S.AbstractClassDef(id)
      case N.CaseClassDef(name, fields, _) =>
        val Some((id,sig)) = table.getConstructor(module,name)
        if(fields.size != sig.argTypes.size) {
          fatal(s"Wrong number of arguments for constructor $name", df.position)
        }
        S.CaseClassDef(
          id,
          sig.argTypes.map(t => S.TypeTree(t))
            .zip(fields)
            .map{case (tt0, tt1)=>tt0.setPos(tt1)},
          sig.parent)
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    def transformLiteral(lit: N.Literal[_]) = {
      lit match {
        case N.IntLiteral(i) => S.IntLiteral(i)
        case N.BooleanLiteral(b) => S.BooleanLiteral(b)
        case N.StringLiteral(s) => S.StringLiteral(s)
        case N.UnitLiteral() => S.UnitLiteral()
      }
    }
    def getName(qname: N.QualifiedName, module: String) = {
      val owner = qname.module match {
        case Some(s) => s
        case None => module
      }
      (owner,qname.name)
    }
    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
              case N.WildcardPattern() => (S.WildcardPattern(), Nil)
              case N.IdPattern(name) => 
                val s = Identifier.fresh(name);
                (S.IdPattern(s), List((name,s)))
              case N.LiteralPattern(lit) => (S.LiteralPattern(transformLiteral(lit)), Nil)
              case N.CaseClassPattern(qname, fields) => 
                val (owner, name) = getName(qname, module);
                val id = table.getConstructor(owner, name) match {
                  case Some((sym,_)) => sym
                  case None => 
                    fatal(s"Could not find constructor $qname", pat.position)
                }
                val (newFields, newNames) = fields.map(transformPattern).unzip;
                val finalNames = newNames.flatten;
                finalNames.groupBy(_._1).foreach{ case (name,names) =>
                  if(names.size > 1) {
                    fatal(s"Duplicate name $name in pattern", pat.position)
                  }
                }
                (S.CaseClassPattern(id,newFields), finalNames)


            }
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            val newExpr = transformExpr(rhs)(module, (params,locals++moreLocals))
            S.MatchCase(newPat, newExpr)
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        // Literals
        case lt:N.Literal[_] => transformLiteral(lt)

        // Binary Operations
        case N.Plus(lhs,rhs) => S.Plus(transformExpr(lhs),transformExpr(rhs))
        case N.Minus(lhs,rhs) => S.Minus(transformExpr(lhs), transformExpr(rhs))
        case N.Times(lhs,rhs) => S.Times(transformExpr(lhs), transformExpr(rhs))
        case N.Div(lhs,rhs) => S.Div(transformExpr(lhs), transformExpr(rhs))
        case N.Mod(lhs,rhs) => S.Mod(transformExpr(lhs), transformExpr(rhs))
        case N.LessThan(lhs,rhs) => S.LessThan(transformExpr(lhs), transformExpr(rhs))
        case N.LessEquals(lhs,rhs) => S.LessEquals(transformExpr(lhs), transformExpr(rhs))
        case N.And(lhs,rhs) => S.And(transformExpr(lhs), transformExpr(rhs))
        case N.Or(lhs,rhs) => S.Or(transformExpr(lhs), transformExpr(rhs))
        case N.Equals(lhs,rhs) => S.Equals(transformExpr(lhs), transformExpr(rhs))
        case N.Concat(lhs, rhs) => S.Concat(transformExpr(lhs), transformExpr(rhs))

        // Unary Operations 
        case N.Not(e) => S.Not(transformExpr(e))
        case N.Neg(e) => S.Neg(transformExpr(e))

        case N.Call(qname, args) =>
          val (owner,name) = getName(qname, module);
          val id = (table.getFunction(owner,name), table.getConstructor(owner,name)) match {
            case (Some((sym,sig)),_) => sym
            case (_, Some((sym,sig))) => sym
            case _ =>
              fatal(s"Could not find function or constructor $name in module $owner", expr)
          }

          S.Call(id, args.map(transformExpr))
        case N.Sequence(e1,e2) => S.Sequence(transformExpr(e1),transformExpr(e2))
        case N.Let(df, value, body) => {
          
          val s = Identifier.fresh(df.name)
          val sParamDef = S.ParamDef(s, S.TypeTree(transformType(df.tt, module)).setPos(df.tt));
          if(params contains df.name) {
            warning(s"Value definition ${df.name} shadows function parameter",
              df.position);
          }
          S.Let(sParamDef,
            transformExpr(value),
            transformExpr(body)(module, (params, locals + (df.name -> s)))
            )
        }
        case N.Ite(cond, thenn, elze) =>
          S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case N.Error(msg) => S.Error(transformExpr(msg))
        case N.Variable(name) => 
          val iden = (params.get(name), locals.get(name)) match {
            case (Some(_),Some(id)) => id
            case (None, Some(id)) => id
            case (Some(id), None) => id
            case (None, None) =>
              fatal(s"Value $name undeclared in scope", expr.position)
          }
          S.Variable(iden)
      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
