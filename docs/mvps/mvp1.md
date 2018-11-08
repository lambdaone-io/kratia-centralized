# MVP 1

## Target users
the Lambda One developers

## General
A simple interface that allows users to see a list of decisions stated as simple YES/NO questions. If the ballot box is
closed then the result of the decision can be seen, if not the user can set his vote (changing the vote is open until the
ballot box closes).

## Features
1. A user is able to start using the system by only registering with a nickname.
2. A user is able to start a decision by posting a YES/NO question, the system automatically starts a collector with a
ballot box with a closing date.
3. After the closing date, a ballot is considered closed and the collector should not accept new votes.
4. On the main interface, the user should see the form to register with his name, a form to post a new decision, and a
list of decisions with related addresses used to query the related collector.
5. When querying for a collector with future closing date, then offer the possibility to cast or change the user vote.
6. When querying for a collector with past closing date, then display the resolution.
7. The community is a common one with a global address.
8. The influence distribution function is Pure Democracy.
9. The decision resolution function is Majority.
10. The collector closing time range is taken from a configuration (something like 2 days)

## Achievements of the MVP approach
Show a primitive but complete flow for proposing-voting-resolving decisions using the model.

Makes us (developers of Kratia) experiment and live the pain points and issues involved in collaborative decision making.

## Value provided
**issue 1:** Collecting votes is a manual process that requires a person to manually collect votes, count them, and
publish the resolution.

**solution 1:** Lambda one members can use this tool already for voting asynchronously on further decisions (like the
specifications of the next MVPs), removing the necessity of a person to manually collect votes.

**issue 2:** Once a decision has been made, either a member must remember the resolution or someone has to manually
record it.

**solution 2:** Once the collection time has ended and the ballot is closed, the resolution is automatically recorded
and displayed on a central historical repository, removing the necessity of a person to manually record and keep a
historical record of the resolved decisions.

## Next step after this MVP
Adding several extra different influence distribution functions and an option to change the used influence distribution
function by the community. To appreciate how by changing this, the resolved decisions are taken also change.

## Medium term steppes after this MVP
Develop the features that will allow us as Kratia developers, be able to develop Kratia in a decentralized manner, where
no central person is needed for taking decisions unless implicitly stated by the chosen decision system. This may include
adding webhooks or API calls like accepting pull requests on GitHub.