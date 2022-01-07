package AST.GumTree

import AST.Parse.Parser
import AST.TestUtils.getIdGenerator
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class MinimumEditScriptTest extends AnyWordSpecLike with Matchers {
  def assertSuccessTransformation(from_source: String, to_source: String): Unit = {
    val getIdentity = getIdGenerator
    val from = Parser.parse(from_source, getIdentity).get
    val to = Parser.parse(to_source, getIdentity).get

    val mapping = GumTreeAlgorithm(from, to).mappings(from.root.get, to.root.get)

    val editScript = new MinimumEditScript(from, to, mapping).compute()

    val from_transformed = editScript.foldLeft(from)((tree, edit) => tree perform edit)
    val transformed_string = from_transformed.toAstString()
    val to_string = to.toAstString()

    assert(from_transformed(from_transformed.root.get) isomorphic(from_transformed, to(to.root.get), to))
    assert(transformed_string == to_string)
  }


  "Basic" in {
    assertSuccessTransformation("((a))", "(a)")
  }

  "transformations" in {
    assertSuccessTransformation(
      "(define a 10)",
      "(define b 10)"
    )
    assertSuccessTransformation(
      "(define a b)",
      "(define b a)"
    )
    assertSuccessTransformation(
      "(define c a b)",
      "(define b a)"
    )
    assertSuccessTransformation(
      "(begin (define a 10) (define c 20) c a b)",
      "(define b a)"
    )
    assertSuccessTransformation(
      "(begin (define a 10) (define c 20) c a b)",
      "(define b a)"
    )
    assertSuccessTransformation(
      "(begin)",
      "(define b a)"
    )

    assertSuccessTransformation("(((c) (d)))", "(((e) (d)))")

    // src: https://riptutorial.com/scheme/example/10903/quicksort
    assertSuccessTransformation(
      """
        (define (quicksort lst)
          (cond
            ((or (isnull lst)
                 (isnull (cdr lst)))
             lst)
            (else
              (let ((pivot (car lst))
                    (rest (cdr lst)))
                (append
                  (quicksort
                    (filter (lambda (x) (lt x pivot)) rest))
                  (list pivot)
                  (quicksort
                    (filter (lambda (x) (gte x pivot)) rest)))))))
        """,
      """
        (define (quicksort lst)
          (cond
            ((or (isnull lst)
                 (isnull (cdr lst)))
             lst)
            (else
              (let ((pivot (car lst))
                    (rest (cdr lst)))
                (append
                  (quicksort
                    (filter (lambda (x) (lt x pivot)) rest))
                  (list pivot)
                  (quicksort
                    (filter (lambda (y) (gte x pivot)) rest)))))))
        """)
    assertSuccessTransformation("(a)", "((a))")
    assertSuccessTransformation("()", "(a)")
    assertSuccessTransformation("(a)", "((a) ((((a)))) (a))")
  }
}
