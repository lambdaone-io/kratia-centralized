
The Basic Model
===============

**Keywords** _community, member, registry, decision, event, domain, influence, proposal, vote, ballot, resolution, resolution outcome._

Let us describe informally what it means to make collaborative decision-making. 

First a notion of **community** as a set of **members** is required, all of the decision
power is encapsulated within such community. Secondly, the community must have a 
**registry** to authenticate and authorise members. Following, **decisions** may be triggered
by incoming **events** which are categorised by **domain**. Then a separation is made
between the **influence** that each member has on the decision, and the way in which a
decision is **resolved** into a **resolution outcome** after **votes** over **proposals** have
been collected in a **ballot box**, the voting **ballot** represents not a vote count on a
proposal, but allocated influence of the voting member over the different proposals. 
Proposals might be predefined or previously collected, but we will leave that for a 
future section. Influence distribution and decision resolution are actually just functions, 
called the **influence distribution function** and the **decision resolution function**, and
these together are called the **decision system functions**.

It is desired to have modular parts of the model, which when changed, the community's way of deciding changes, and subsequentially the community changes, these parts must be formalized or semi-formalized mathematically in order to have a full understanding of them, be able to reduce it to unambiguous code, be able to measure its impact, and be able to proof properties or extend them by providing ways of combining them and compose them. 

This documentation will attempt to do such semi formalization of the parts of the model and will use the programming language Haskell as the chosen tool since it stands in between programmers and mathematicians. With this we would like to achieve some kind of denotational semantics, so to cater to the mathematical rigor as much as to the programming implementation and computational challenges.

* Usage of tagless final

The first parts that we will semi formalize, are the concept of a community, a member and a registry, allowing us to continue with the decision system functions, which are of uttermost importance since they stand at the core of Kratia.

## Member, Community, and the Registry

* Community as a set of members
* Importance of handling without loading all data 
* Registry as a polymorphic operation

The community can be delimited by the registry, which is the module that dictates membership, we will use a type class to describe the module.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

type Address = String

data Community a = CommunityRef Address

data Member a = MemberRef Address

class Registry f a where
  isMember :: Community a -> a -> f Bool
```

## Influence Distribution

When decisions are to be made, a community might want to have a notion of influence distribution, so that more capable or more affected individuals have more decision power over others. Given the set of members of a community `M` and a measurement of influence `Infl`, the influence distribution is a function that maps every member with its influence amount. 

```haskell
dist :: M -> Infl
```

* Required relation with our notion of community and registry

```haskell
type Influence = Rational

class InfluenceDistribution f method where
  alloc :: Registry f a => Community a -> a -> method -> f Influence
```
