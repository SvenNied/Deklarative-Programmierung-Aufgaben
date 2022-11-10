import GHC.CmmToAsm.AArch64.Instr (x0)
data JSON = JNull
          | JBool Bool
          | JInt Int
          | JFloat Float
          | JString String
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving Show

exampleJSON :: JSON
exampleJSON = JArray [
    JObject[
            ("name", JString "meier"),
            ("besuchte_kurse", JArray [JString "Logik", JString "Programmierung", JString "Compilerbau"]),
            ("bachelor_note", JNull),
            ("zugelassen", JBool True)
        ],
        JObject [
            ("name", JString "schmidt"),
            ("besuchte_kurse", JArray [JString "Programmierung", JString "Informationssysteme"]),
            ("bachelor_note", JFloat 2.7),
            ("zugelassen", JBool False)
        ]
    ]

foldJson:: a -> (Bool -> a) -> (Int -> a) -> (Float -> a) -> (String -> a) -> ([a] -> a) -> ([(String, a)] -> a) -> JSON -> a
foldJson fNull fBool fInt fFloat fString fArray fObject input = case input of
    JNull -> fNull
    JBool x -> fBool x
    JInt x -> fInt x
    JFloat x -> fFloat x
    JString x -> fString x
    JArray x -> fArray (map (foldJson fNull fBool fInt fFloat fString fArray fObject) x)
    JObject x -> fObject ( map (\(x,y) -> (x, (foldJson fNull fBool fInt fFloat fString fArray fObject y))) x)

stringify :: JSON -> String
stringify = foldJson "null" show show show (\x-> x) (\x -> "[" ++ show x ++ "]") (\x -> "{" ++ show x ++ "}")