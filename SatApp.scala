object SatApp {
  def main(args: Array[String]): Unit = {
    val kb = KnowledgeBase.parseFromInput()
    val solver = new SatSolver()
    val result = solver.solve(kb)
    print(result.asString)
  }
}
