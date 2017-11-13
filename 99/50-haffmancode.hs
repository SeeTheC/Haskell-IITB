import Data.List
data Btree a = Null | Btree a (Btree a) (Btree a) deriving (Show,Eq)


--We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:

 --Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
--[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]



huffman l = [(x,encodeChar x ht)|  (x,_)<-l ]
            where tl = [ Btree ([c],w) Null Null | (c,w)<-l] 
                  ht=head$huffmanTree tl

huffmanTree tl | length tl == 1 = tl
               | otherwise= huffmanTree $ mergeFirstTwo sortedList    
                 where
                 sortedList=sortNode tl
                 mergeFirstTwo (n1@(Btree (s1,w1) _ _):n2@(Btree (s2,w2) _ _):xs) = (Btree (s1++s2,w1+w2) n1 n2):xs

sortNode tl = sortBy comparater tl
comparater (Btree (_,w1) _ _) (Btree (_,w2) _ _) = compare w1 w2


-- strict binary tree: Every node will have 2 or zero children
encodeChar c (Btree (s,w) Null Null) = []
encodeChar c (Btree (s,w) lt@(Btree (sl,_) _ _) rt) | (c `notElem` s) = [] 
                                                    | otherwise= if c `elem` sl then '0': encodeChar c lt else '1': encodeChar c rt
                                    
