-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Opaleye.Internal.HaskellDB.Sql.Print ( 
                                     ppUpdate,
                                     ppDelete, 
                                     ppInsert,
                                     ppSqlExpr,
                                     ppWhere,
                                     ppGroupBy,
                                     ppOrderBy,
                                     ppAs,
                                     commaV,
                                     commaH
	                            ) where

import Opaleye.Internal.HaskellDB.Sql (SqlColumn(..), SqlDelete(..),
                               SqlExpr(..), SqlOrder(..), SqlInsert(..),
                               SqlUpdate(..))
import qualified Opaleye.Internal.HaskellDB.Sql as Sql

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), ($$), (<>), comma, doubleQuotes,
                                  empty, equals, hcat, hsep, parens, punctuate,
                                  text, vcat)


ppWhere :: [SqlExpr] -> Doc
ppWhere [] = empty
ppWhere es = text "WHERE" 
             <+> hsep (intersperse (text "AND")
                       (map (parens . ppSqlExpr) es))

ppGroupBy :: [SqlExpr] -> Doc
ppGroupBy es = text "GROUP BY" <+> ppGroupAttrs es
  where
    ppGroupAttrs :: [SqlExpr] -> Doc
    ppGroupAttrs cs = commaV nameOrExpr cs
    nameOrExpr :: SqlExpr -> Doc
    nameOrExpr (ColumnSqlExpr (SqlColumn col)) = text col
    nameOrExpr expr = parens (ppSqlExpr expr)
    
ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy [] = empty
ppOrderBy ord = text "ORDER BY" <+> commaV ppOrd ord
    where
      ppOrd (e,o) = ppSqlExpr e <+> ppSqlDirection o <+> ppSqlNulls o

ppSqlDirection :: Sql.SqlOrder -> Doc
ppSqlDirection x = text $ case Sql.sqlOrderDirection x of
  Sql.SqlAsc  -> "ASC"
  Sql.SqlDesc -> "DESC"

ppSqlNulls :: Sql.SqlOrder -> Doc
ppSqlNulls x = text $ case Sql.sqlOrderNulls x of
        Sql.SqlNullsFirst -> "NULLS FIRST"
        Sql.SqlNullsLast  -> "NULLS LAST"

ppAs :: String -> Doc -> Doc
ppAs alias expr    | null alias    = expr                               
                   | otherwise     = expr <+> (hsep . map text) ["as",alias]


ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate name assigns criteria)
        = text "UPDATE" <+> text name
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
    where
      ppAssign (c,e) = ppColumn c <+> equals <+> ppSqlExpr e


ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete name criteria) =
    text "DELETE FROM" <+> text name $$ ppWhere criteria


ppInsert :: SqlInsert -> Doc

ppInsert (SqlInsert table names values)
    = text "INSERT INTO" <+> text table 
      <+> parens (commaV ppColumn names)
      $$ text "VALUES" <+> parens (commaV ppSqlExpr values)

-- If we wanted to make the SQL slightly more readable this would be
-- one easy place to do it.  Currently we wrap all column references
-- in double quotes in case they are keywords.  However, we should be
-- sure that any column names we generate ourselves are not keywords,
-- so we only need to double quote base table column names.
ppColumn :: SqlColumn -> Doc
ppColumn (SqlColumn s) = doubleQuotes (text s)


ppSqlExpr :: SqlExpr -> Doc
ppSqlExpr expr =
    case expr of
      ColumnSqlExpr c     -> ppColumn c
      ParensSqlExpr e -> parens (ppSqlExpr e)
      BinSqlExpr op e1 e2 -> ppSqlExpr e1 <+> text op <+> ppSqlExpr e2 
      PrefixSqlExpr op e  -> text op <+> ppSqlExpr e
      PostfixSqlExpr op e -> ppSqlExpr e <+> text op
      FunSqlExpr f es     -> text f <> parens (commaH ppSqlExpr es)
      AggrFunSqlExpr f es     -> text f <> parens (commaH ppSqlExpr es)
      ConstSqlExpr c      -> text c
      CaseSqlExpr cs el   -> text "CASE" <+> vcat (map ppWhen cs)
                             <+> text "ELSE" <+> ppSqlExpr el <+> text "END"
          where ppWhen (w,t) = text "WHEN" <+> ppSqlExpr w 
                               <+> text "THEN" <+> ppSqlExpr t
      ListSqlExpr es      -> parens (commaH ppSqlExpr es)
      ParamSqlExpr _ v -> ppSqlExpr v
      PlaceHolderSqlExpr -> text "?"
      CastSqlExpr typ e -> text "CAST" <> parens (ppSqlExpr e <+> text "AS" <+> text typ)
    

commaH :: (a -> Doc) -> [a] -> Doc
commaH f = hcat . punctuate comma . map f

commaV :: (a -> Doc) -> [a] -> Doc
commaV f = vcat . punctuate comma . map f
