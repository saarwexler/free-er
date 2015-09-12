import MonadContinuation.MonadContinuation

import scala.collection.mutable

/*
data Union (r :: [ * -> * ]) v where
  UNow  :: t v -> Union (t ': r) v
  UNext :: Union r v -> Union (any ': r) v
 */

sealed trait Union[R <: T forSome { type T <: MonadContinuation[M, _, _] forSome { type M[_] } }, V] {
  type MCList = mutable.ListBuffer[Class[R]]
}
case class UNow[T[_], V, R](t: T[V], u: Union[R, V]):

/*
-- injecting/projecting at a specified position P n
class Member' t r (n :: Nat) where
inj' :: P n -> t v -> Union r v
prj' :: P n -> Union r v -> Maybe (t v)
*/

trait Member_ {
  //def inj_[T[_], V](n: Int, t: T[V]}): Union
  //def prj_[]
}

/*
class Member t r => MemberU2 (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (MemberU' (EQU t1 t2) tag t1 (t2 ': r)) => MemberU2 tag t1 (t2 ': r)

class Member t r =>
    MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' True tag (tag e) (tag e ': r)
instance (Member t (t' ': r), MemberU2 tag t r) =>
         MemberU' False tag t (t' ': r)
 */