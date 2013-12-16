package io.segl

import scala.language.experimental.macros
import scala.reflect.macros.Context
import org.slf4j.LoggerFactory

object SeglMacros {

  def hello(name: String): Unit = macro helloImpl

  def trace(param: Any): Unit = macro traceImpl

  def info(param: Any): Unit = macro infoImpl

  def helloImpl(c: Context)(name: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._ // always import the universe at first in each macro
    reify { println(s"Hello ${name.splice}!") }
    // reify takes ordinary code and convert it to the corresponding AST expression
  }

  def traceImpl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._ // always import the universe at first in each macro
    val paramRep = show(param.tree)
    val paramRepExpr = c.Expr[String](Literal(Constant(paramRep)))
    reify { println(paramRepExpr.splice + " = " + param.splice) }
  }

  def infoImpl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._ // always import the universe at first in each macro

    // Interesting bit:
    // ValDef(Modifiers(PRIVATE), newTermName("logger "), TypeTree(), Apply(Select(Ident(newTermName("LoggerFactory")), newTermName("getLogger")), List(Literal(Constant("macros")))))
    // println(showRaw(c.enclosingClass))
    // enclosingClass gives the AST of the class or object where the macro was called

    val body = c.enclosingClass match {
      case ModuleDef(_, _, Template(_, _, b)) => b
      case unknown =>
        c.abort(c.enclosingPosition, s"Unknown type of enclosing class: ${unknown.getClass}")
    }

    val typeAndName = body.collect {
      case ValDef(_, termName, t, _) => t.tpe.toString -> termName
    }

    val foundLogger = typeAndName.find(_._1 == "org.slf4j.Logger")

    val loggerTermName = foundLogger.getOrElse(
      c.abort(c.enclosingPosition, s"Could not find field of type 'org.slf4j.Logger' in enclosing class")
    )._2

    // val logger = LoggerFactory.getLogger("test")
    // println(showRaw(reify(logger.info("test"))))

    // c.Expr[Unit](Apply(Select(Ident(newTermName("logger")), newTermName("info")), List(Literal(Constant("test")))))

    val paramRep = show(param.tree)
    val paramRepExpr = c.Expr[String](Literal(Constant(paramRep)))

    val logger = LoggerFactory.getLogger("test")
    println(showRaw(reify(if(logger.isInfoEnabled) logger.info("test"))))

//    c.Expr[Unit](Apply(Select(Ident(newTermName("logger")), newTermName("info")), List(
//      reify(paramRepExpr.splice + " = " + param.splice).tree
//    )))

    c.Expr[Unit](If(Apply(Select(Ident(loggerTermName), newTermName("isInfoEnabled")), List()), Apply(Select(Ident(newTermName("logger")), newTermName("info")), List(
      // Literal(Constant("test")))), Literal(Constant(()))
      reify(paramRepExpr.splice + " = " + param.splice).tree)), Literal(Constant(()))
      // , Literal(Constant(()))
    ))

  }

}
