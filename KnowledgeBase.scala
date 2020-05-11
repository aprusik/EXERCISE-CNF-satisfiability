import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

class KnowledgeBase(val numVars: Int, numClauses: Int,
  var clauses: mutable.Set[List[Int]] = mutable.Set.empty,
  var vars: mutable.Set[Int] = mutable.Set.empty,
  var satisfiable: Int = -1)
{

  def copy: KnowledgeBase = {
    new KnowledgeBase(numVars, numClauses, clauses.clone(), vars.clone(), satisfiable)
  }

  def addClause(clause: List[Int]): Unit = {
    clauses += clause
  }

//  def unitPropagate(): Unit = {
//    print("")
//    vars ++= clauses.collect{case c if c.size == 1 => clauses -= c; c.head}
//    print("")
//  }

  def setVariable(v: Int): KnowledgeBase = {
    vars += v
    clauses = clauses.collect{
      // remove -v from all clauses with -v
      case c if c.contains(-v) => c.filter(_ != -v)
      // only include clauses that don't have v
      case c if !c.contains(v) => c
    }
    this
  }

  def asString: String = {
    var s = ""
    val explored = KnowledgeBase.explored
    vars.foreach{v => s += s"v $v\n"}
    s"s cnf $satisfiable $numVars $numClauses\n" + s +
      s"$explored branching nodes explored."
  }
}

object KnowledgeBase {
  var explored = 0

  def parseFromInput(): KnowledgeBase = {

    var newKB: Option[KnowledgeBase] = Option.empty

    var clauses = 1
    while (0 < clauses) {
      val line: List[Any] = StdIn.readLine.split(" ").toList.map(
        (input: String) => {try input.toInt catch {case _: Exception => input}}
      )

      line match {
        // comment line; ignore
        case "c" :: _ => Unit
        // prepare line; declare number of variables and clauses
        case "p" :: "cnf" :: (numVars: Int) :: (numClauses: Int) :: _ =>
          clauses = numClauses
          newKB = Some(new KnowledgeBase(numVars, numClauses))
        case "p" :: _ =>
          throw formatException("Usage: p cnf <num_variables> <num_clauses>")
        // clause line; declare clause
        case clauseLine =>
          if (newKB.isDefined) {
            val clause = clauseLine.collect{case v: Int => v}

            @tailrec
            def parseClause(clause: List[Int]): Unit = {
              val split = clause.span(_ != 0)
//              println(split._1)
              clauses -= 1
              newKB.get.addClause(split._1)
              if (split._2.tail != Nil) parseClause(split._2.tail)
            }
            parseClause(clause)
          }
          else throw formatException("Must use p to define number of variables " +
            "and clauses before defining a clause.")
      }
    }

    if (newKB.isDefined) newKB.get
    else throw formatException("KB not defined")
  }

  private def formatException(msg: String): IllegalArgumentException =
    new IllegalArgumentException("Input must be in standard DIMACS CNF" +
      "format.  " + msg)
}
