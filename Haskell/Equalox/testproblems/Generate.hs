import System.Process



generateAll 0 = return ()
generateAll n = do writeProblem n 
                   generateAll (n-1)

writeProblem :: Int -> IO ()
writeProblem n = let file = ("test_" ++ show n) in do
  writeFile file (generateProblem n) 
  toCnf file

generateEqRel n = generateTrans n ++ "\n" ++ generateRefl n ++ "\n" ++ generateSymm n ++ "\n"
generateTrans n = "fof(transitive_" ++ show n ++ ", axiom, ![X,Y,Z]: " ++ isTransitive n ++ ")."
generateRefl n = "fof(reflexive_" ++ show n ++ ", axiom, ![X]: " ++ isReflexive n ++ ")."
generateSymm n =  "fof(symmetric_" ++ show n ++ ", axiom, ![X,Y]: " ++ isSymmetric n ++ ")."

generateProblem :: Int -> String
generateProblem n = generateRelations n ++ generateDef (n+1) ++ generateConjecture (n+1)

generateRelations 0 = ""
generateRelations n = generateEqRel n ++ generateRelations (n-1)

rel n xs = "r_" ++ show n ++ xs

isTransitive n = "((" ++ rel n "(X,Y) & " ++ rel n "(Y,Z)) => " ++ rel n "(X,Z))"
isReflexive n  = "(" ++ rel n "(X,X))"
isSymmetric n  = "(" ++ rel n "(X,Y) => " ++ rel n "(Y,X))"


generateDef n = "fof(def,axiom, ![X,Y]: (" ++ rel n "(X,Y)"  ++ " <=> (" ++ def (n-1) ++ "))).\n"


def :: Int -> String
def 1 = rel 1 "(X,Y)" 
def n = rel n "(X,Y)" ++ " | " ++ def (n-1)


generateConjecture n = "fof(c,conjecture, " ++ "![X,Y,Z] : ((" ++ isTransitive n ++ ") & (" ++ isReflexive n ++ ") & (" ++ isSymmetric n ++ ")))."


generateConjecture2 n = "fof(c,conjecture, " ++ "![X,Y,Z] : ((" ++ isTransitive n ++ ") & (" ++ isReflexive n ++ ") & (" ++ isSymmetric n ++ ")))."

toCnf file = callCommand $ "jukebox cnf " ++ file ++ " > " ++ file ++ "_cnf"