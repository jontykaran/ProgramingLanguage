{-# OPTIONS_GHC -w #-}
module	MyParser where	
import	MyLexer
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

action_0 (9) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (9) = happyShift action_2
action_1 _ = happyFail

action_2 (15) = happyShift action_6
action_2 (5) = happyGoto action_7
action_2 _ = happyFail

action_3 (21) = happyAccept
action_3 _ = happyFail

action_4 (15) = happyShift action_6
action_4 (5) = happyGoto action_5
action_4 _ = happyFail

action_5 (10) = happyShift action_10
action_5 _ = happyFail

action_6 (19) = happyShift action_9
action_6 _ = happyFail

action_7 (10) = happyShift action_8
action_7 _ = happyFail

action_8 (15) = happyShift action_12
action_8 (6) = happyGoto action_14
action_8 _ = happyFail

action_9 (16) = happyShift action_13
action_9 _ = happyFail

action_10 (15) = happyShift action_12
action_10 (6) = happyGoto action_11
action_10 _ = happyFail

action_11 (11) = happyShift action_20
action_11 (12) = happyShift action_21
action_11 (15) = happyShift action_22
action_11 (18) = happyShift action_15
action_11 (7) = happyGoto action_18
action_11 (8) = happyGoto action_19
action_11 _ = happyFail

action_12 (13) = happyShift action_17
action_12 _ = happyReduce_7

action_13 (20) = happyShift action_16
action_13 _ = happyReduce_5

action_14 (18) = happyShift action_15
action_14 _ = happyFail

action_15 _ = happyReduce_1

action_16 (15) = happyShift action_6
action_16 (5) = happyGoto action_30
action_16 _ = happyFail

action_17 (15) = happyShift action_12
action_17 (6) = happyGoto action_29
action_17 _ = happyFail

action_18 (14) = happyShift action_27
action_18 (18) = happyShift action_28
action_18 _ = happyFail

action_19 (18) = happyShift action_26
action_19 _ = happyFail

action_20 (15) = happyShift action_25
action_20 _ = happyFail

action_21 (15) = happyShift action_6
action_21 (5) = happyGoto action_24
action_21 _ = happyFail

action_22 (19) = happyShift action_23
action_22 _ = happyFail

action_23 (16) = happyShift action_33
action_23 _ = happyFail

action_24 _ = happyReduce_12

action_25 (19) = happyShift action_32
action_25 _ = happyFail

action_26 _ = happyReduce_3

action_27 (12) = happyShift action_21
action_27 (8) = happyGoto action_31
action_27 _ = happyFail

action_28 _ = happyReduce_2

action_29 _ = happyReduce_8

action_30 _ = happyReduce_6

action_31 (18) = happyShift action_36
action_31 _ = happyFail

action_32 (16) = happyShift action_35
action_32 _ = happyFail

action_33 (17) = happyShift action_34
action_33 _ = happyFail

action_34 (15) = happyShift action_6
action_34 (5) = happyGoto action_38
action_34 _ = happyFail

action_35 (17) = happyShift action_37
action_35 _ = happyFail

action_36 _ = happyReduce_4

action_37 (15) = happyShift action_6
action_37 (5) = happyGoto action_39
action_37 _ = happyFail

action_38 _ = happyReduce_9

action_39 (13) = happyShift action_40
action_39 _ = happyReduce_10

action_40 (11) = happyShift action_20
action_40 (15) = happyShift action_22
action_40 (7) = happyGoto action_41
action_40 _ = happyFail

action_41 _ = happyReduce_11

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Choose happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (MChoose happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 6 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (OChoose happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 8 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (OMChoose happy_var_2 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyTerminal (TokenInt _ happy_var_3))
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn5
		 (SCol happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 5 5 happyReduction_6
happyReduction_6 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (MCol happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn6
		 (STab happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn6
		 (MTab happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 7 happyReduction_9
happyReduction_9 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ClauseEquality happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 6 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ClauseSuchThat happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 8 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ClauseMulSuchThat happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (ClauseOrderBy happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 21 21 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenChoose _ -> cont 9;
	TokenFrom _ -> cont 10;
	TokenSuchThat _ -> cont 11;
	TokenOrderBy _ -> cont 12;
	TokenAnd _ -> cont 13;
	TokenAs _ -> cont 14;
	TokenVar _ happy_dollar_dollar -> cont 15;
	TokenInt _ happy_dollar_dollar -> cont 16;
	TokenEq _ -> cont 17;
	TokenEOS _ -> cont 18;
	TokenDot _ -> cont 19;
	TokenComma _ -> cont 20;
	_ -> happyError' (tk:tks)
	}

happyError_ 21 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError	::	[Token]	->	a
parseError	[]	=	error	"Parse error *** Empty File"	

--DEFINING ALL THE DATATYPES

data Query = Choose Col Table | MChoose Col Table Exp | OChoose Col Table Order | OMChoose Col Table Exp Order deriving (Show,Eq)
data Col =  SCol String Int | MCol String Int Col deriving (Show,Eq)
data Table = STab String | MTab String Table deriving (Show,Eq)
data Exp = ClauseEquality String Int Col | ClauseSuchThat String Int Col | ClauseMulSuchThat String Int Col Exp deriving (Eq,Show)
data Order = ClauseOrderBy Col deriving (Eq,Show)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc7776_0\\ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
