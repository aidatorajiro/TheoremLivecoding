/**
 * Create A -> B.
 */
function create_a_b (a, b) {
   return (x) => {
      if (x === a) {
        return b;
      } else {
        throw new Error('type mismatch');
      }
   }
}

/**
 * proof of (A -> B) -> ((B -> C) -> (A -> C))
 */
prf = (ab) => ((bc) => ((a) => (bc(ab(a)))))

// the prop (A -> B) -> ((B -> C) -> (A -> C))
prop1 = (prf) => ((prf(create_a_b('A', 'B')))(create_a_b('B', 'C')))("A") === "C" // -> True