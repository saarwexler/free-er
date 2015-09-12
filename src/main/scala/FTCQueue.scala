// From http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs

/*
data FTCQueue m a b where
  Leaf :: (a -> m b) -> FTCQueue m a b
  Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
 */

object MonadContinuation {
  type MonadContinuation[M[_], A, B] = A => M[B]
}

import MonadContinuation.MonadContinuation

sealed trait FTCQueue[M[_], A, B]
case class Leaf[M[_], A, B](r: MonadContinuation[M, A, B]) extends FTCQueue[M, A, B]
case class Node[M[_], A, B, X](t: FTCQueue[M, A, X], r: FTCQueue[M, X, B]) extends FTCQueue[M, A, B]

object FTCQueue {
  /*
tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton r = Leaf r
 */

  def tsingleton[M[_], A, B](r: MonadContinuation[M, A, B]): FTCQueue[M, A, B] = Leaf(r)

  /* snoc
  (|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
    t |> r = Node t (Leaf r)
  */

  def |>[M[_], A, B, X](t: FTCQueue[M, A, X], r: MonadContinuation[M, X, B]): FTCQueue[M, A, B]  = Node(t, Leaf(r))

  /* append
  (><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
t1 >< t2 = Node t1 t2
   */
  def ><[M[_], A, B, X](t1: FTCQueue[M, A, X], t2: FTCQueue[M, X, B]): FTCQueue[M, A, B] = Node(t1, t2)

  /*
  data ViewL m a b where
  TOne  :: (a -> m b) -> ViewL m a b
  (:|)  :: (a -> m x) -> (FTCQueue m x b) -> ViewL m a b
   */

  sealed trait ViewL[M[_], A, B]
  case class TOne[M[_], A, B](r: MonadContinuation[M, A, B]) extends ViewL[M, A, B]
  case class :|[M[_], A, B, X](r: MonadContinuation[M, A, X], t: FTCQueue[M, X, B]) extends ViewL[M, A, B]

  /*
  tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r) = TOne r
tviewl (Node t1 t2) = go t1 t2
 where
   go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
   go (Leaf r) tr = r :| tr
   go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)
   */

  object ViewL {
    def tviewl[M[_], A, B](q: FTCQueue[M, A, B]): ViewL[M, A, B] = {

      def go[M[_], A, B, X](t1: FTCQueue[M, A, X], t2: FTCQueue[M, X, B]): ViewL[M, A, B] = t1 match {
        case Leaf(r) => :|(r, t2)
        case Node(tl1, tl2) => go(tl1, Node(tl2, t2))
      }

      q match {
        case Leaf(r) => TOne(r)
        case Node(tl1, tl2) => go(tl1, tl2)
      }
    }
  }
}