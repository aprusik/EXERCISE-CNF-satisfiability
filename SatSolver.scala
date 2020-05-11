class SatSolver() {

  def solve(kb: KnowledgeBase): KnowledgeBase = {
    unitPropagate(kb)
    if (kb.clauses.isEmpty) {
      kb.satisfiable = 1
      kb
    }
    else if (kb.clauses.exists(_.isEmpty)) {
      kb.satisfiable = 0
      kb
    }
    else {
      KnowledgeBase.explored += 1
      val possVars =
        (1 to kb.numVars).filter(v => !kb.vars.exists(v2 => v2 == v || v2 == -v))
//      println(possVars.size)
      val v = possVars.head
      solve(kb.copy.setVariable(v)) match {
        case newKb if newKb.satisfiable == 1 => newKb
        case _ => solve(kb.copy.setVariable(-v))
      }
    }
  }

  private def unitPropagate(kb: KnowledgeBase): Unit = {
    var c: Option[List[Int]] = kb.clauses.find(_.size == 1)
    while ({c = kb.clauses.find(_.size == 1); c.isDefined}) { // maybe shouldn't be while loop?
//    if (c.isDefined)
      kb.setVariable(c.get.head)
    }
  }
}
