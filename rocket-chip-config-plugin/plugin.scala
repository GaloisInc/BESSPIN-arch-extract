package com.galois.besspin

import java.io._
import scala.reflect.runtime.universe._
import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class RocketChipConfigPlugin(val global: Global) extends Plugin {
  import global._

  val name = "rocket-chip-config"
  val description = "lists rocket-chip configuration options"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: RocketChipConfigPlugin.this.global.type = RocketChipConfigPlugin.this.global
    val runsAfter = List[String]("refchecks")
    val phaseName = RocketChipConfigPlugin.this.name
    def newPhase(_prev: Phase) = new RocketChipConfigPhase(_prev)
    class RocketChipConfigPhase(prev: Phase) extends StdPhase(prev) {
      override def name = Component.this.phaseName
      def apply(unit: CompilationUnit): Unit = {
        traverser.traverse(unit.body)
        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body
             if rcvr.tpe <:< definitions.IntClass.tpe)
          {
            global.reporter.error(tree.pos, "don't do that")
          }
      }

      val configName = "freechips.rocketchip.config.Config"

      object traverser extends Traverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case cd: ClassDef =>
              val isConfigClass = cd.impl.parents.exists(
                _.symbol.fullName == configName)
              if (isConfigClass) {
                Worker.handleClassDef(cd)
              }
              super.traverse(tree)
            case _ => super.traverse(tree)
          }
        }
      }
    }
  }

  private object Worker {
    val configsFile = new PrintWriter(new File("config-classes.txt"))
    val fieldsFile = new PrintWriter(new File("config-fields.txt"))

    val fieldName = "freechips.rocketchip.config.Field";

    def handleClassDef(cd: ClassDef) {
      for (DefDef(_, termNames.CONSTRUCTOR, _, args, _, body) <- cd.impl.body) {
        // Is this a boolean config option, either present or not?
        // (Some config options take an integer argument.)
        val isBoolean = args == List(List())
        // Is this a "simple" config option, implemented as a single
        // function?  (Some config options are compositions of
        // multiple other options.)
        var isSimple = false;
        for (Apply(Select(Super(This(_), typeNames.EMPTY),
            termNames.CONSTRUCTOR), List(arg)) <- body) {
          isSimple = arg match {
            case Function(_, _) => true
            case _ => false
          }
        }

        if (isBoolean && isSimple) {
          global.reporter.echo(s"found config class ${cd.symbol.fullName}")
          configsFile.println(cd.symbol.fullName)
          configsFile.flush()
        }

        if (isSimple) {
          // PartialFunction has two methods, applyOrElse and
          // isDefinedAt.  We look at the latter because its cases
          // return only true and false - there's no risk of catching
          // a case's body expression with the inner pattern below.
          for (DefDef(_, TermName("isDefinedAt"), _, _, _, fnBody) <- body) {
            for (If(Apply(Select(pat, TermName("$eq$eq")), List(Ident(_))), _, _) <- fnBody) {
              if (pat.tpe.baseClasses.exists(base => base.fullName == fieldName)) {
                global.reporter.echo(s"found config key ${pat.symbol.fullName}")
                fieldsFile.println(pat.symbol.fullName)
                fieldsFile.flush()
              }
            }
          }
        }
      }
    }
  }
}
