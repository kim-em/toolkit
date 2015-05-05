{{HoldPattern[cachedGroebnerBasis[ideal_, variables_]] :> 
   (cachedGroebnerBasis[ideal, variables] = GroebnerBasis[ideal, 
     variables])}, 
 {HoldPattern[cachedPolynomialReduce[polynomials_List, basis_, 
     variables_]] :> (cachedPolynomialReduce[#1, basis, variables] & ) /@ 
    polynomials, HoldPattern[cachedPolynomialReduce[polynomial:Except[_List], 
     basis_, variables_]] :> (cachedPolynomialReduce[polynomial, basis, 
     variables] = PolynomialReduce[polynomial, basis, variables])}}
