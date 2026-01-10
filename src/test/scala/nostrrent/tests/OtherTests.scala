package nostrrent.tests

import org.scalatest.funsuite.AnyFunSuite
import nostrrent.NostrrentID

class OtherTests
extends AnyFunSuite:

  test("Verify NostrrentID"):
    val id1 = NostrrentID()
    val idStr = id1.toString()
    idStr match
      case NostrrentID() =>
        val id2 = NostrrentID(idStr)
        assert(id1 === id2)
      case _ => fail(s"`$idStr` failed to match as NostrrentID")
