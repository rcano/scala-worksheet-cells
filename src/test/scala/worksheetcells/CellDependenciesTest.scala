package worksheetcells

import scala.concurrent.Future

import munit.GenericAfterEach

class CellDependenciesTest extends munit.FunSuite {
  
  override def afterEach(context: GenericAfterEach[Future[Any]]): Unit = {
    Gwms.clear()
  }

  test("time based reevaluation") {
    val baseCell = Cell("base") {
      "base"
    }
    val depCell = baseCell.mapTimeBased(_ => math.random())
    val captured = depCell.value
    baseCell.reeval()
    assertNotEquals(captured, depCell.value)
  }

  test("hash based reevaluation: when contents did not change") {
    val baseCell = Cell("base") {
      "base"
    }
    val depCell = baseCell.map(_ => math.random())
    val captured = depCell.value
    baseCell.reeval()
    assertEquals(captured, depCell.value)
  }

  test("hash based reevaluation: when contents changed") {
    val baseCell = Cell("base") {
      math.random()
    }
    val depCell = baseCell.map(_ => math.random())
    val captured = depCell.value
    baseCell.reeval()
    assertNotEquals(captured, depCell.value)
  }
}
