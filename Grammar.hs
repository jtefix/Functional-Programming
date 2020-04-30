{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,282) ([0,20480,4608,0,1024,0,0,32768,0,0,320,72,0,2048,0,0,512,0,4,2048,0,0,32,8192,2,2568,36352,24576,674,8704,32768,176,2176,9728,40,544,2432,10,0,0,0,34,41112,0,12288,64,0,0,0,57344,255,0,0,0,0,2176,8192,40,0,0,0,0,0,0,34,41117,1,0,512,0,0,8,0,768,4,0,192,1,31744,0,4,0,80,18,16,0,0,0,32,0,124,8,0,0,2,0,0,0,0,0,0,0,0,0,16,0,0,1984,1024,0,34,41088,32768,8,10272,8192,2,2568,34816,0,642,8704,32768,160,0,2048,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1152,0,5120,1156,1024,0,0,2176,9728,40,544,2432,10,0,2048,0,0,512,0,0,2048,8192,2,2568,0,768,4,1024,0,8,0,0,0,544,2048,10,136,33280,2,34,41088,32768,8,10272,8192,2,2568,34816,0,642,0,0,2,0,320,72,7936,0,0,1984,0,0,496,0,0,124,0,0,31,0,49152,7,0,8704,32768,176,0,0,0,7936,0,1,0,8192,0,0,8197,1,16384,18433,0,0,0,0,0,0,8704,32768,160,0,0,0,0,512,0,0,128,0,35,45184,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1792,0,0,448,0,0,0,0,0,31,4,0,0,64,0,0,0,0,0,0,0,0,0,1984,128,0,0,9221,1,16384,18689,0,0,256,0,0,0,61440,1,16,0,320,73,0,0,0,16,0,0,0,256,0,32768,0,0,0,0,0,0,0,0,8192,0,2176,8192,40,0,0,0,0,0,0,0,512,0,0,0,0,20480,4608,0,5120,1168,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Exp","ShortExp","BoolValues","MathExp","list","Type","TypeList","EmptyList","Bool","Int","int","'='","'!='","'+'","'-'","'*'","'/'","'%'","'<'","'<='","'>'","'>='","'=='","'&&'","'||'","if","else","while","true","false","';'","','","'('","')'","'{'","'}'","'['","']'","var","ReadStream","sizeOf","output","%eof"]
        bit_start = st * 46
        bit_end = (st + 1) * 46
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..45]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (29) = happyShift action_4
action_0 (31) = happyShift action_5
action_0 (42) = happyShift action_6
action_0 (45) = happyShift action_7
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (29) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (36) = happyShift action_14
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (29) = happyShift action_4
action_3 (31) = happyShift action_5
action_3 (42) = happyShift action_6
action_3 (45) = happyShift action_7
action_3 (46) = happyAccept
action_3 (4) = happyGoto action_13
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (36) = happyShift action_12
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (36) = happyShift action_11
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (15) = happyShift action_9
action_6 (40) = happyShift action_10
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (36) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (14) = happyShift action_18
action_8 (18) = happyShift action_19
action_8 (36) = happyShift action_28
action_8 (42) = happyShift action_23
action_8 (44) = happyShift action_24
action_8 (7) = happyGoto action_37
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (12) = happyShift action_34
action_9 (13) = happyShift action_35
action_9 (14) = happyShift action_18
action_9 (18) = happyShift action_19
action_9 (32) = happyShift action_20
action_9 (33) = happyShift action_21
action_9 (36) = happyShift action_28
action_9 (40) = happyShift action_36
action_9 (42) = happyShift action_23
action_9 (44) = happyShift action_24
action_9 (6) = happyGoto action_30
action_9 (7) = happyGoto action_31
action_9 (9) = happyGoto action_32
action_9 (10) = happyGoto action_33
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (14) = happyShift action_18
action_10 (18) = happyShift action_19
action_10 (36) = happyShift action_28
action_10 (41) = happyShift action_29
action_10 (42) = happyShift action_23
action_10 (44) = happyShift action_24
action_10 (7) = happyGoto action_27
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (14) = happyShift action_18
action_11 (18) = happyShift action_19
action_11 (32) = happyShift action_20
action_11 (33) = happyShift action_21
action_11 (36) = happyShift action_22
action_11 (42) = happyShift action_23
action_11 (44) = happyShift action_24
action_11 (5) = happyGoto action_26
action_11 (6) = happyGoto action_16
action_11 (7) = happyGoto action_17
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (14) = happyShift action_18
action_12 (18) = happyShift action_19
action_12 (32) = happyShift action_20
action_12 (33) = happyShift action_21
action_12 (36) = happyShift action_22
action_12 (42) = happyShift action_23
action_12 (44) = happyShift action_24
action_12 (5) = happyGoto action_25
action_12 (6) = happyGoto action_16
action_12 (7) = happyGoto action_17
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (4) = happyGoto action_13
action_13 _ = happyReduce_12

action_14 (14) = happyShift action_18
action_14 (18) = happyShift action_19
action_14 (32) = happyShift action_20
action_14 (33) = happyShift action_21
action_14 (36) = happyShift action_22
action_14 (42) = happyShift action_23
action_14 (44) = happyShift action_24
action_14 (5) = happyGoto action_15
action_14 (6) = happyGoto action_16
action_14 (7) = happyGoto action_17
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (27) = happyShift action_51
action_15 (28) = happyShift action_52
action_15 (37) = happyShift action_66
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_23

action_17 (16) = happyShift action_60
action_17 (17) = happyShift action_38
action_17 (18) = happyShift action_39
action_17 (19) = happyShift action_40
action_17 (20) = happyShift action_41
action_17 (21) = happyShift action_42
action_17 (22) = happyShift action_61
action_17 (23) = happyShift action_62
action_17 (24) = happyShift action_63
action_17 (25) = happyShift action_64
action_17 (26) = happyShift action_65
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_34

action_19 (14) = happyShift action_18
action_19 (18) = happyShift action_19
action_19 (36) = happyShift action_28
action_19 (42) = happyShift action_23
action_19 (44) = happyShift action_24
action_19 (7) = happyGoto action_59
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_24

action_21 _ = happyReduce_25

action_22 (14) = happyShift action_18
action_22 (18) = happyShift action_19
action_22 (29) = happyShift action_4
action_22 (31) = happyShift action_5
action_22 (32) = happyShift action_20
action_22 (33) = happyShift action_21
action_22 (36) = happyShift action_22
action_22 (42) = happyShift action_58
action_22 (44) = happyShift action_24
action_22 (45) = happyShift action_7
action_22 (4) = happyGoto action_49
action_22 (5) = happyGoto action_57
action_22 (6) = happyGoto action_16
action_22 (7) = happyGoto action_17
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (40) = happyShift action_56
action_23 _ = happyReduce_35

action_24 (36) = happyShift action_55
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (27) = happyShift action_51
action_25 (28) = happyShift action_52
action_25 (37) = happyShift action_54
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (27) = happyShift action_51
action_26 (28) = happyShift action_52
action_26 (37) = happyShift action_53
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (17) = happyShift action_38
action_27 (18) = happyShift action_39
action_27 (19) = happyShift action_40
action_27 (20) = happyShift action_41
action_27 (21) = happyShift action_42
action_27 (41) = happyShift action_50
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (29) = happyShift action_4
action_28 (31) = happyShift action_5
action_28 (42) = happyShift action_6
action_28 (45) = happyShift action_7
action_28 (4) = happyGoto action_49
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (15) = happyShift action_48
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (34) = happyShift action_47
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (17) = happyShift action_38
action_31 (18) = happyShift action_39
action_31 (19) = happyShift action_40
action_31 (20) = happyShift action_41
action_31 (21) = happyShift action_42
action_31 (34) = happyShift action_46
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (34) = happyShift action_45
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_41

action_34 _ = happyReduce_39

action_35 _ = happyReduce_40

action_36 (13) = happyShift action_44
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (17) = happyShift action_38
action_37 (18) = happyShift action_39
action_37 (19) = happyShift action_40
action_37 (20) = happyShift action_41
action_37 (21) = happyShift action_42
action_37 (37) = happyShift action_43
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (14) = happyShift action_18
action_38 (18) = happyShift action_19
action_38 (36) = happyShift action_28
action_38 (42) = happyShift action_23
action_38 (44) = happyShift action_24
action_38 (7) = happyGoto action_94
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (14) = happyShift action_18
action_39 (18) = happyShift action_19
action_39 (36) = happyShift action_28
action_39 (42) = happyShift action_23
action_39 (44) = happyShift action_24
action_39 (7) = happyGoto action_93
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (14) = happyShift action_18
action_40 (18) = happyShift action_19
action_40 (36) = happyShift action_28
action_40 (42) = happyShift action_23
action_40 (44) = happyShift action_24
action_40 (7) = happyGoto action_92
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (14) = happyShift action_18
action_41 (18) = happyShift action_19
action_41 (36) = happyShift action_28
action_41 (42) = happyShift action_23
action_41 (44) = happyShift action_24
action_41 (7) = happyGoto action_91
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (14) = happyShift action_18
action_42 (18) = happyShift action_19
action_42 (36) = happyShift action_28
action_42 (42) = happyShift action_23
action_42 (44) = happyShift action_24
action_42 (7) = happyGoto action_90
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (34) = happyShift action_89
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (41) = happyShift action_88
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_6

action_46 _ = happyReduce_4

action_47 _ = happyReduce_5

action_48 (40) = happyShift action_86
action_48 (43) = happyShift action_87
action_48 (10) = happyGoto action_84
action_48 (11) = happyGoto action_85
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (29) = happyShift action_4
action_49 (31) = happyShift action_5
action_49 (37) = happyShift action_83
action_49 (42) = happyShift action_6
action_49 (45) = happyShift action_7
action_49 (4) = happyGoto action_13
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (15) = happyShift action_82
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (14) = happyShift action_18
action_51 (18) = happyShift action_19
action_51 (32) = happyShift action_20
action_51 (33) = happyShift action_21
action_51 (36) = happyShift action_22
action_51 (42) = happyShift action_23
action_51 (44) = happyShift action_24
action_51 (5) = happyGoto action_81
action_51 (6) = happyGoto action_16
action_51 (7) = happyGoto action_17
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (14) = happyShift action_18
action_52 (18) = happyShift action_19
action_52 (32) = happyShift action_20
action_52 (33) = happyShift action_21
action_52 (36) = happyShift action_22
action_52 (42) = happyShift action_23
action_52 (44) = happyShift action_24
action_52 (5) = happyGoto action_80
action_52 (6) = happyGoto action_16
action_52 (7) = happyGoto action_17
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (38) = happyShift action_79
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (38) = happyShift action_78
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (42) = happyShift action_77
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (14) = happyShift action_18
action_56 (18) = happyShift action_19
action_56 (36) = happyShift action_28
action_56 (42) = happyShift action_23
action_56 (44) = happyShift action_24
action_56 (7) = happyGoto action_76
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (27) = happyShift action_51
action_57 (28) = happyShift action_52
action_57 (37) = happyShift action_75
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (15) = happyShift action_9
action_58 (40) = happyShift action_74
action_58 _ = happyReduce_35

action_59 _ = happyReduce_31

action_60 (14) = happyShift action_18
action_60 (18) = happyShift action_19
action_60 (36) = happyShift action_28
action_60 (42) = happyShift action_23
action_60 (44) = happyShift action_24
action_60 (7) = happyGoto action_73
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (14) = happyShift action_18
action_61 (18) = happyShift action_19
action_61 (36) = happyShift action_28
action_61 (42) = happyShift action_23
action_61 (44) = happyShift action_24
action_61 (7) = happyGoto action_72
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (14) = happyShift action_18
action_62 (18) = happyShift action_19
action_62 (36) = happyShift action_28
action_62 (42) = happyShift action_23
action_62 (44) = happyShift action_24
action_62 (7) = happyGoto action_71
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (14) = happyShift action_18
action_63 (18) = happyShift action_19
action_63 (36) = happyShift action_28
action_63 (42) = happyShift action_23
action_63 (44) = happyShift action_24
action_63 (7) = happyGoto action_70
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (14) = happyShift action_18
action_64 (18) = happyShift action_19
action_64 (36) = happyShift action_28
action_64 (42) = happyShift action_23
action_64 (44) = happyShift action_24
action_64 (7) = happyGoto action_69
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (14) = happyShift action_18
action_65 (18) = happyShift action_19
action_65 (36) = happyShift action_28
action_65 (42) = happyShift action_23
action_65 (44) = happyShift action_24
action_65 (7) = happyGoto action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (38) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (29) = happyShift action_4
action_67 (31) = happyShift action_5
action_67 (42) = happyShift action_6
action_67 (45) = happyShift action_7
action_67 (4) = happyGoto action_107
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (17) = happyShift action_38
action_68 (18) = happyShift action_39
action_68 (19) = happyShift action_40
action_68 (20) = happyShift action_41
action_68 (21) = happyShift action_42
action_68 _ = happyReduce_18

action_69 (17) = happyShift action_38
action_69 (18) = happyShift action_39
action_69 (19) = happyShift action_40
action_69 (20) = happyShift action_41
action_69 (21) = happyShift action_42
action_69 _ = happyReduce_17

action_70 (17) = happyShift action_38
action_70 (18) = happyShift action_39
action_70 (19) = happyShift action_40
action_70 (20) = happyShift action_41
action_70 (21) = happyShift action_42
action_70 _ = happyReduce_16

action_71 (17) = happyShift action_38
action_71 (18) = happyShift action_39
action_71 (19) = happyShift action_40
action_71 (20) = happyShift action_41
action_71 (21) = happyShift action_42
action_71 _ = happyReduce_15

action_72 (17) = happyShift action_38
action_72 (18) = happyShift action_39
action_72 (19) = happyShift action_40
action_72 (20) = happyShift action_41
action_72 (21) = happyShift action_42
action_72 _ = happyReduce_14

action_73 (17) = happyShift action_38
action_73 (18) = happyShift action_39
action_73 (19) = happyShift action_40
action_73 (20) = happyShift action_41
action_73 (21) = happyShift action_42
action_73 _ = happyReduce_19

action_74 (14) = happyShift action_18
action_74 (18) = happyShift action_19
action_74 (36) = happyShift action_28
action_74 (41) = happyShift action_29
action_74 (42) = happyShift action_23
action_74 (44) = happyShift action_24
action_74 (7) = happyGoto action_106
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_22

action_76 (17) = happyShift action_38
action_76 (18) = happyShift action_39
action_76 (19) = happyShift action_40
action_76 (20) = happyShift action_41
action_76 (21) = happyShift action_42
action_76 (41) = happyShift action_105
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (40) = happyShift action_104
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (29) = happyShift action_4
action_78 (31) = happyShift action_5
action_78 (42) = happyShift action_6
action_78 (45) = happyShift action_7
action_78 (4) = happyGoto action_103
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (29) = happyShift action_4
action_79 (31) = happyShift action_5
action_79 (42) = happyShift action_6
action_79 (45) = happyShift action_7
action_79 (4) = happyGoto action_102
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_21

action_81 _ = happyReduce_20

action_82 (14) = happyShift action_18
action_82 (18) = happyShift action_19
action_82 (36) = happyShift action_28
action_82 (42) = happyShift action_23
action_82 (44) = happyShift action_24
action_82 (7) = happyGoto action_101
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_32

action_84 (34) = happyShift action_100
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (34) = happyShift action_99
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (13) = happyShift action_44
action_86 (14) = happyShift action_18
action_86 (18) = happyShift action_19
action_86 (36) = happyShift action_28
action_86 (41) = happyShift action_98
action_86 (42) = happyShift action_23
action_86 (44) = happyShift action_24
action_86 (7) = happyGoto action_96
action_86 (8) = happyGoto action_97
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (34) = happyShift action_95
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_42

action_89 _ = happyReduce_13

action_90 _ = happyReduce_30

action_91 _ = happyReduce_29

action_92 _ = happyReduce_28

action_93 (19) = happyShift action_40
action_93 (20) = happyShift action_41
action_93 (21) = happyShift action_42
action_93 _ = happyReduce_27

action_94 (19) = happyShift action_40
action_94 (20) = happyShift action_41
action_94 (21) = happyShift action_42
action_94 _ = happyReduce_26

action_95 _ = happyReduce_8

action_96 (17) = happyShift action_38
action_96 (18) = happyShift action_39
action_96 (19) = happyShift action_40
action_96 (20) = happyShift action_41
action_96 (21) = happyShift action_42
action_96 (35) = happyShift action_115
action_96 _ = happyReduce_37

action_97 (41) = happyShift action_114
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_43

action_99 _ = happyReduce_10

action_100 _ = happyReduce_7

action_101 (17) = happyShift action_38
action_101 (18) = happyShift action_39
action_101 (19) = happyShift action_40
action_101 (20) = happyShift action_41
action_101 (21) = happyShift action_42
action_101 (34) = happyShift action_113
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (29) = happyShift action_4
action_102 (31) = happyShift action_5
action_102 (39) = happyShift action_112
action_102 (42) = happyShift action_6
action_102 (45) = happyShift action_7
action_102 (4) = happyGoto action_13
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (29) = happyShift action_4
action_103 (31) = happyShift action_5
action_103 (39) = happyShift action_111
action_103 (42) = happyShift action_6
action_103 (45) = happyShift action_7
action_103 (4) = happyGoto action_13
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (41) = happyShift action_110
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_33

action_106 (17) = happyShift action_38
action_106 (18) = happyShift action_39
action_106 (19) = happyShift action_40
action_106 (20) = happyShift action_41
action_106 (21) = happyShift action_42
action_106 (41) = happyShift action_109
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (29) = happyShift action_4
action_107 (31) = happyShift action_5
action_107 (39) = happyShift action_108
action_107 (42) = happyShift action_6
action_107 (45) = happyShift action_7
action_107 (4) = happyGoto action_13
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyFail (happyExpListPerState 108)

action_109 (15) = happyShift action_82
action_109 _ = happyReduce_33

action_110 (37) = happyShift action_119
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (30) = happyShift action_118
action_111 _ = happyReduce_1

action_112 _ = happyReduce_3

action_113 _ = happyReduce_11

action_114 (34) = happyShift action_117
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (14) = happyShift action_18
action_115 (18) = happyShift action_19
action_115 (36) = happyShift action_28
action_115 (42) = happyShift action_23
action_115 (44) = happyShift action_24
action_115 (7) = happyGoto action_96
action_115 (8) = happyGoto action_116
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_38

action_117 _ = happyReduce_9

action_118 (38) = happyShift action_120
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_36

action_120 (29) = happyShift action_4
action_120 (31) = happyShift action_5
action_120 (42) = happyShift action_6
action_120 (45) = happyShift action_7
action_120 (4) = happyGoto action_121
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (29) = happyShift action_4
action_121 (31) = happyShift action_5
action_121 (39) = happyShift action_122
action_121 (42) = happyShift action_6
action_121 (45) = happyShift action_7
action_121 (4) = happyGoto action_13
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_2

happyReduce_1 = happyReduce 7 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfStmt happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 11 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfElseStmt happy_var_3 happy_var_6 happy_var_10
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 7 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (WhileExp happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Assignment happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 4 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Assignment happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 4 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (TypeAssignment happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 6 4 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (TypeAssignment happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 4 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (StreamRead happy_var_1
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 8 4 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Assignment happy_var_1 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 6 4 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Assignment happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 7 4 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IndexAssignment happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (App happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 4 happyReduction_13
happyReduction_13 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Output happy_var_3
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (LessOrEqThan happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (BiggerThan happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (BiggerOrEqThan happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (IsEq happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (NotEq happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (And happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  5 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Or happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  5 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  5 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  6 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn6
		 (LanTrue
	)

happyReduce_25 = happySpecReduce_1  6 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn6
		 (LanFalse
	)

happyReduce_26 = happySpecReduce_3  7 happyReduction_26
happyReduction_26 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  7 happyReduction_27
happyReduction_27 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  7 happyReduction_28
happyReduction_28 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  7 happyReduction_29
happyReduction_29 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Div happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  7 happyReduction_30
happyReduction_30 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Mod happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  7 happyReduction_31
happyReduction_31 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Negate happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  7 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 7 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (IndexOf happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_1  7 happyReduction_34
happyReduction_34 (HappyTerminal (TokenInt _ happy_var_1))
	 =  HappyAbsSyn7
		 (LanInt happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  7 happyReduction_35
happyReduction_35 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn7
		 (LanVar happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happyReduce 6 7 happyReduction_36
happyReduction_36 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (SizeOf happy_var_3
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_1  8 happyReduction_37
happyReduction_37 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (SingleList happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  8 happyReduction_38
happyReduction_38 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (MultipleList happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  9 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn9
		 (TypeBool
	)

happyReduce_40 = happySpecReduce_1  9 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn9
		 (TypeInt
	)

happyReduce_41 = happySpecReduce_1  9 happyReduction_41
happyReduction_41 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  10 happyReduction_42
happyReduction_42 _
	_
	_
	 =  HappyAbsSyn10
		 (TypeList
	)

happyReduce_43 = happySpecReduce_2  11 happyReduction_43
happyReduction_43 _
	_
	 =  HappyAbsSyn11
		 (EmptyList
	)

happyNewToken action sts stk [] =
	action 46 46 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenTypeBool _ -> cont 12;
	TokenTypeInt _ -> cont 13;
	TokenInt _ happy_dollar_dollar -> cont 14;
	TokenEq _ -> cont 15;
	TokenNotEq _ -> cont 16;
	TokenPlus _ -> cont 17;
	TokenMinus _ -> cont 18;
	TokenMult _ -> cont 19;
	TokenDiv _ -> cont 20;
	TokenMod _ -> cont 21;
	TokenLessThan _ -> cont 22;
	TokenLessOrEqThan _ -> cont 23;
	TokenBiggerThan _ -> cont 24;
	TokenBiggerOrEqThan _ -> cont 25;
	TokenIsEq _ -> cont 26;
	TokenAnd _ -> cont 27;
	TokenOr _ -> cont 28;
	TokenIf _ -> cont 29;
	TokenElse _ -> cont 30;
	TokenWhile _ -> cont 31;
	TokenTrue _ -> cont 32;
	TokenFalse _ -> cont 33;
	TokenSemiCol _ -> cont 34;
	TokenComma _ -> cont 35;
	TokenLRoundB _ -> cont 36;
	TokenRRoundB _ -> cont 37;
	TokenLCurlyB _ -> cont 38;
	TokenRCurlyB _ -> cont 39;
	TokenLSquareB _ -> cont 40;
	TokenRSquareB _ -> cont 41;
	TokenVar _ happy_dollar_dollar -> cont 42;
	TokenReadStream _ -> cont 43;
	TokenSizeOf _ -> cont 44;
	TokenOutput _ -> cont 45;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 46 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- error function
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:_) = error ("Parse error at line:column " ++ ( tokenPosn t))

data Exp = App Exp Exp
         | Assignment String Exp
         | TypeAssignment String Type
         | Plus Exp Exp
         | Minus Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Mod Exp Exp
         | Negate Exp
         | LessThan Exp Exp
         | LessOrEqThan Exp Exp
         | BiggerThan Exp Exp
         | BiggerOrEqThan Exp Exp
         | IsEq Exp Exp
         | NotEq Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | LanTrue
         | LanFalse
         | LanInt Int
         | LanVar String
         | IfStmt Exp Exp
         | IfElseStmt Exp Exp Exp
         | WhileExp Exp Exp
         | StreamRead String
         | IndexAssignment String Exp Exp
         | IndexOf String Exp
         | SingleList Exp
         | MultipleList Exp Exp
         | EmptyList
         | SizeOf String
         | Output Exp
    deriving (Show, Eq)

data Type = TypeInt | TypeBool | TypeList
      deriving (Show, Eq)

type Environment = [ (String,Exp) ]
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc1900_0/ghc_2.h" #-}


























































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

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

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

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
happyFail explist i tk (HappyState (action)) sts stk =
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

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
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
