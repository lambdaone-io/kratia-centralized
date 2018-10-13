Decision System Functions Combinators
=====================================

Influence distribution monoid
-----------------------------

```scala

case class InfluenceDistribution[M, I](run: M => I)

implicit def monoid[M, I](implicit mi: Monoid[I]): Monoid[InfluenceDistribution[M, I]] =
  new Monoid[InfluenceDistribution[M, I]] {

    override def empty: InfluenceDistribution[M, I] =
      InfluenceDistribution(_ => mi.empty)

    override def combine(x: InfluenceDistribution[M, I], y: InfluenceDistribution[M, I]): InfluenceDistribution[M, I] =
      InfluenceDistribution(m => mi.combine(x.run(m), y.run(m)))
  }

```
