module Main(main) where 
import Data.List
import Data.Char
import MyLexer
import MyParser

-- BINDING / CROSSPROD OF TWO TABLE  
crossProd :: [[String]] -> [[String]] -> [[String]]
crossProd xs ys = intermcross [ (x, y)| x <- xs  , y <- ys ]
                   where 
                      intermcross :: [([a],[a])] -> [[a]]
                      intermcross [] = []   
                      intermcross (x:xs) = (fst x ++ snd x ): intermcross xs
 
-- PARSE TABLE / READ CSV FILE  
inc :: String -> [[String]]
inc xs = [splitBy ',' a | a<- (splitBy '\n' xs)]
--split in lines and comma 
splitBy :: Char -> String -> [String]
splitBy _ "" = [];
splitBy delimiterChar inputString = foldr f [""] inputString
  where f :: Char -> [String] -> [String]
        f currentChar allStrings@(partialString:handledStrings)
          | currentChar == delimiterChar = "":allStrings           -- start a new partial string at the head of the list of all strings
          | otherwise = (currentChar:partialString):handledStrings -- add the current char to the partial string

-- CHOOSE QUERY TYPE FROM THE PARSE STATEMENT  				
choosequery:: Query -> Int
choosequery (Choose colm tabl )= 1
choosequery (MChoose colm tabl ex) = 2
choosequery (OChoose colm tabl or) = 3
choosequery (OMChoose colm tabl ex or) = 4

-- RETURN SIZE OF LIST OF LIST i.e. number of columns in table				
funcSize :: [[String]] -> Int 
funcSize (x:xs) = length(x)

--TRANSPOSE OF THE MATRIX 
convert:: [[String]]->[[String]]
convert ([]:_) = []
convert x = (map head x) : convert (map tail x)

---------------------------------------------------------------------------------------------------------------------
-- # FUNCTIONS TO EXTRACT VALUES FROM DIFFERENT DATA TYPE -> usable in functions further  
--	EXTRACT COLUMN VALUES FROM COL DATA TYPE
dot :: Col -> [(String,Int)]
dot (SCol x y) = (x,y):[]
dot (MCol x y z) = (x,y): dot z

--READ A QUERY TO EXTRACT COLUMN NUMBERS FOR SELECTION
attr :: Query -> [(String,Int)]
attr (Choose colm tabl ) = dot (colm)
attr (MChoose colm tabl ex) = dot ( colm )
attr (OChoose colm tabl ord) = dot ( colm )
attr (OMChoose colm tabl ex ord) = dot ( colm )

-- EXTRACT LIST OF TABLES FROM THE TABLE DATA TYPE  
table :: Table -> [String]
table (STab a) = [a]
table (MTab a b) = [a] ++ table b

-- READ A QUERY TO EXTRACT TABLE NAME 
file :: Query -> [String]
file (Choose colm tabl ) = table (tabl)
file (MChoose colm tabl ex) = table ( tabl )
file (OChoose colm tabl or) = table ( tabl )
file (OMChoose colm tabl ex or) = table ( tabl )

-- EXTRACT THE COLUMN NUMBERS FROM EXP DATA TYPE 
giveexpr :: Exp -> [(String,Int)]
giveexpr (ClauseEquality a b c) = [(a,b)] ++ dot (c)
giveexpr (ClauseSuchThat a b c) = [(a,b)] ++ dot (c)
giveexpr (ClauseMulSuchThat a b c d) = [(a,b)] ++ dot (c) ++ giveexpr d 

-- READ A QUERY TO EXTRACT COLUMN NUMBER FOR EQUALITY 
equalityattr :: Query -> [(String,Int)]
equalityattr (MChoose colm tabl exp) = giveexpr (exp) 
equalityattr (OMChoose colm tabl exp ord) = giveexpr (exp) 

-- EXTRACT THE COLUMN NUMBER FROM ORD DATA TYPE 
orderBy :: Order -> (String,Int)
orderBy (ClauseOrderBy colm) = head (dot (colm))

-- READ A QUERY TO ECTRACT COLUMN NUMBER FOR ORDERBY 
orderByAttr :: Query -> (String,Int) 
orderByAttr (OChoose colm tabl ord) = orderBy (ord)
orderByAttr (OMChoose colm tabl exp ord) = orderBy (ord)
---------------------------------------------------------------------------------------------------------------------

-- # FUNCTIONS TO SUPPORT THE QUERY LANGUAGE TO GET DESIERED OUTPUT 
-- SELECTION ON THE BASIS OF COLUMN NUMBER IN THE EXPRESSION 
select :: [(String,Int)] -> [[String]] -> Query -> Int ->[[String]]
select [] yss qu size = []
select (x:xs) yss qu size| fst x == file qu!!0 = yss!!((snd x) -1) : select (xs) yss qu size
                         | fst x == file qu!!1 = yss!!(((size+1) + ((snd x)-1)) - 1 ) : select (xs) yss qu size

-- FUNCTION TO GET TABLE AFTER CHECKING EQUALITY ON THE COLUMNS ENTERED
equate :: [(String,Int)] -> [[String]] -> Query -> Int -> [[String]]
equate ys xs q x = [a | a <- xs, equate' ys a q x]
                  where
                      equate' :: [(String, Int)] -> [String] -> Query -> Int -> Bool
                      equate' [] _ _ _ = True
                      equate' (_:[]) _ _ _ = True
                      equate' (t1:t2:ts) ls qu x = ls !! index1 == ls !! index2 && equate' (t2:ts) ls qu x
                                                  where
                                                      index1 = if fst t1 == file qu!!0 then (snd t1) - 1 else (x + (snd t1)-1)
                                                      index2 = if fst t2 == file qu!!0 then (snd t2) - 1 else (x + (snd t2)-1)

-- FUNCTION TO GET A TABLE AFTER APPLYING ORDER BY CONDITION ON A PARTICULAR COLUMN 				
order :: [[String]] -> (String,Int) -> Query -> Int -> [[String]]
order xss (t,c) q size | t == file q!!0 = concat[[ a | a <- (xss) ,((a!!(c-1))== b)] | b <- redrem(sor (xss) (t,c) q size) ] 
                       | t == file q!!1 && (length (file q) > 1)= concat[[ a | a <- (xss) ,(a!!(((size+1) + (c)-1) - 1)==b)] | b <- redrem(sor (xss) (t,c) q size) ]
-- FACILITATING FUNCTION FOR ORDER FUNCTION
sor :: [[String]] -> (String,Int) -> Query -> Int -> [String]
sor xss value q size| fst value == (file q)!!0 = sort( (convert xss)!!((snd value)-1 ) )
                    | fst value == file q!!1 = sort((convert xss)!!(((size+1) + (snd value)-1) - 1) ) 
-- FACILITATING FUNCTION FOR ORDER FUNCTION  
redrem :: [String] -> [String]
redrem [] =[]
redrem [x] = [x]
redrem (x:y:ys) | x==y = redrem (y:ys)
                | x/=y = x: redrem (y:ys)
---------------------------------------------------------------------------------------------------------------------------------------------------------
-- # FUNCTION TO RUN AND IMPLEMENT TO GET DESIRED TABLE AFTER PASSING THE QUERY 
queryfun :: Query -> IO()
queryfun q = do {
            ;let tab1 = file q!!0 ++".csv"
                 tab2 = if(length(file q) == 1) then (tab1) else ( file q!!1 ++".csv")
            ;read1 <- readFile tab1
            ;read2 <- readFile tab2
            ;let tabA = inc read1
                 tabB = inc read2
                 sTab = if(length(file q) == 1) then (tabA) else crossProd tabA tabB
                 tOrder = (order (sTab) (orderByAttr q) q (funcSize tabA ))
                 tEquality = equate (equalityattr q) (sTab) q (funcSize tabA )
                 tEquOrder = equate (equalityattr q) (tOrder) q (funcSize tabA )
                 finalOut |(choosequery q)== 1 = convert(select (attr q) (convert(sTab)) q (funcSize tabA ))        -- print with selection
                          |(choosequery q)== 2 = convert(select (attr q) (convert(tEquality)) q (funcSize tabA ))   -- print with equality
                          |(choosequery q)== 3 = convert(select (attr q) (convert(tOrder)) q (funcSize tabA ))      -- print with order
                          |(choosequery q)== 4 = convert(select (attr q) (convert(tEquOrder)) q (funcSize tabA ))   -- print with order and equality
            ;putStrLn (concat (intersperse "\n" (comma finalOut)) )
             } 
------------------------------------------------------------------------------------------------------------------------------------------------------------
-- # FUNCTION TO ADD COMMA IN BETWEEN 
comma :: [[String]] -> [String]
comma xss = [concat (intersperse "," a) | a <- xss]


-- # MAIN FUNCTION TO RUN ALL THE FILES TOGETHER 
main = do {
    ;file <- getLine 
    ;contents <- readFile (file)
    ;let s = contents 
         tok = alexScanTokens s
    ;let p = parser tok
    ;queryfun p
    }
