import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite

class HelloSpec extends AnyFunSuite {
  test("Hello should start with H") {
    assert("Hello".startsWith("H"))
  }
}

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }
}

import org.scalatest.funspec.AnyFunSpec

class SetSpec extends AnyFunSpec {

  describe("A Set") {
    describe("when empty") {
      it("should have size 0") {
        assert(Set.empty.size == 0)
      }

      it("should produce NoSuchElementException when head is invoked") {
        assertThrows[NoSuchElementException] {
          Set.empty.head
        }
      }
    }
  }
}
