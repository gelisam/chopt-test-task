Samuel Gélineau's implementation of the CH/OTP Test Task
===


Goal
---

As clarified in a Slack conversation with the technical reviewers, fault tolerance is more important than maximizing the score, but I am free to choose implementations like gossip protocols which do not guarantee that all nodes will return the same output. My understanding is that the important part is this document, the justification: I should state what guarantees I'm aiming for and I should explain how my algorithm achieves them. Since this justification is the most important part, I decided to prove many properties of my algorithm. This is why this document is very long :)

Since I'm a big fan of correctness, I would like to aim for a solution which provably guarantees that all the nodes return the same value in spite of arbitrary network failures. Unfortunately, doing so is impossible (see next section), so I shall settle for the following goal instead: I want a solution which provably guarantees that the values returned by the nodes differ at most by one final message. That is, there are only two values allowed: `m` paired with its score, and `init m` paired with its score.


Impossibility proof
---

After a message has been sent, in the presence of network failures, the message may or may not get delivered, and the sender has no way to know which it was. That is true even with TCP, since the connection can break and the sender has no way to know whether the connection broke while its message was sent or while the receiver's ACK was sent back. Unfortunately that means the only way to guarantee that every node will agree on the output is to return a predefined value, probably the empty list. Here is a proof.

Assume that we have a deterministic algorithm guaranteeing that all the nodes agree regardless of which messages were dropped, and consider a particular execution of the algorithm in which a fixed set of nodes exchange a number of messages and terminate with output `o`. This execution can be represented by a graph indicating causal relationships between events: each send, receive or timeout event points to the next event on the same node, and successfully delivered messages point from the send event of the sender node to the receive event of the receiving node. Clearly such a graph can have no cycles, or that would indicate a causality paradox. So we have a DAG.

The bulk of the proof will consist of finding network conditions under which fewer messages reach their destination, and yet the nodes nevertheless output the same result `o`. By induction on the number of successfully-transmitted messages, this means that the nodes output `o` whether they manage to transmit `n` messages, `n - 1` messages, `n - 2` messages, ..., or zero messages. That is, exchanging messages has no effect on `o`, so the output is a predetermined value.

Choosing network conditions under which fewer messages are exchanged is trivial: we can take any message and decide that the network failed to deliver it. The difficulty is that the node which received this message might have reacted to it by sending other messages or by reacting to a timeout. By dropping the message, we affect all the future actions of this node, which might in turn affect the output chosen by all the nodes. But we need an execution in which all the nodes return `o` in order to apply our induction hypothesis.

In order to prevent this from happening, we need to pick our message more carefully. If we can find one whose receiving node `n` does not later manage to successfully send a message, then we can make sure that dropping the message doesn't affect the other nodes by also dropping any subsequent message which `n` might decide to send as a result of not receiving the message. Since the other nodes receive the same messages and encounter the same timeouts as in our original DAG, they must output the same value `o`, and therefore `n` must output `o` as well since we have assumed that the algorithm makes sure that all the nodes return the same value.

All we need in order to complete the proof is to demonstrate that such a message exists. Choose the message using the following procedure. Pick a message, and if the receiving node subsequently sends a successfully-transmitted message, pick that message instead and repeat the procedure. Since there are no cycles in the DAG, the procedure must eventually terminate with a message whose receiving node does not subsequently manage to send another message, QED.


Consensus algorithms
---

If you have heard of consensus algorithms like Raft and Paxos, you might wonder why the nodes can't simply use one of those algorithms to make sure all the nodes agree on the first message, and then use the algorithm again to pick the second message, and so on.

Unfortunately, consensus algorithms help with the wrong part of the problem. They ensure that when and if a node commits to a message, that message will be the same for every node who commits to a message. They do not ensure that all the nodes commit at the same time. That part is harder than consensus: if multiple nodes could initiate a protocol which is guaranteed to end at the same time on all the nodes, they could choose between N messages by running N copies of the protocol and choosing the message corresponding to the copy which ends first.


The semi-lattice behind the algorithm
---

I am a big fan of LVars and CRDTs, so I would like to model the information each node can have in a semi-lattice. Let's begin by modeling the information required during a single round, that is, until all the nodes agree on what the next message should be.

The most important piece of information is of course the message which all the nodes will eventually agree on. Each node begins with its own randomly-generated message candidate, but in a single round we can only select a single message. Since we are allowed to discard messages and the goal is to maximize the score, the simplest way to combine two messages is to discard one and to keep the one with the highest score. So our strategy is for each node to accumulate every candidate message, in any order and with possible duplicates, and to combine them into a final message by repeatedly combining them using `max`. Since `max` is associative, commutative, and idempotent, as long as all the nodes receive all the candidates, they are guaranteed to agree on the final message.

In order to know when the round is over, the nodes need to know whether they have received the candidates from all the nodes already or whether they should continue to wait for more. To keep track of this information, I plan to use a bit field with one bit per node indicating whether the candidate from that node has already been incorporated into the current `max`-based summary. This information can easily be combined using bit-wise union, also an associative, commutative, and idempotent operation.

Interestingly, this scheme allows a node `n` to complete a round despite only receiving messages from a fraction of the other nodes! Let's call the list of nodes from which `n` did receive a message `ns1`, and let's call the remaining nodes `ns2`. If each node in `ns2` sends a message to at least one node in `ns1`, and each node in `ns1` combines this information with what they already have and subsequently sends the combined information to `n`, then the total information combined by `n` will be a bit field full of ones and a maximum value incorporating the messages from all the nodes. At that point, `n` commits to the maximum message, and begins the next round. A round is considered complete once every node has committed to the message.

By making sure the nodes broadcast their combined information every time they learn something new in addition to broadcasting their initial candidate, we ensure that all the nodes obtain all the information even if some of the nodes cannot communicate with each other directly, as long as the channels which do work properly form a connected graph including all the nodes.

Next, let's consider multiple rounds. Naïvely, it should suffice to send information about all the rounds which a node knows about, and to combine this information element-wise. However, this is very inefficient as the amount of information to transfer grows over time. We can do better.

I want to show that at most two rounds are in progress at any point. Consider the smallest round number `r` such that the round is still in progress, that is, for which some messages have been sent but not all the nodes have committed to a message. We want to show that round `r + 2` has not started yet.

Since a round begins once a node has received all the information from the previous round, commits to a message and sends a candidate for the new round, it suffices to show that no node has all the information from round `r + 1`. And in order to show that, it suffices to show that at least one node has not sent its information about round `r + 1`, because it has not yet committed to a message for round `r`. Since by assumption, round `r` is still in progress, we are done.

Since at most two rounds are in progress, it is unnecessary to exchange information about rounds which are older than that, so it suffices to exchange the current round number, the information about the current round, and the information about the previous round. Furthermore, since the current round has begun, the previous round's bit field is necessarily full of ones, so it is superfluous to transmit that information either. Thus, in order for the nodes to participate in an ever-increasing sequence of rounds, it suffices for the nodes to continuously exchange 4 numbers:

1. the current round number
2. the message chosen in the previous round (if any)
3. the maximum message encountered in the current round so far
4. an integer whose bits are the bit field of the current round

To combine two of those quadruples in an associative, commutative, and idempotent way, we first compare the round numbers: if one is higher, we drop the other quadruple and we keep the one with the higher round number. Otherwise the round numbers are the same and it makes sense to merge the values of the rounds pairwise. The messages for the previous round should already be the same, but for the sake and making this associative, commutative, and idempotent, take the `max`. Also take the `max` of the current round's message and the bit-wise union of the bit field, as before.


Assumptions
--

Each node knows how many nodes there are and which index it has in the list of nodes.

There is a unidirectional channel from every node to every other nodes. Messages sent along that channel can be delivered after an arbitrary amount of time or not at all, they may be duplicated, and delivered out of order. I only assume that messages are received after they are sent, and that if the message is delivered, its contents is untampered.


Basic algorithm
--

Each node knows about its index `n` and begins with the following quadruple:

1. round 0
2. a dummy value for round -1
3. a random candidate for round 0
4. bit `n` set to 1

We then broadcast this initial quadruple to every output channel.

Every time we commit to a message and bump to round `r + 1`, we replace the quadruple with:

1. round `r + 1`
2. the message to which we just committed for round `r`
3. a random candidate for round `r + 1`
4. bit `n` set to 1

When a quadruple is received via one of the input channels, it is combined with the current quadruple using the combination function from the previous section. If doing so caused the round number to go up, it must be because we were at round `r` and we received a quadruple from round `r + 1`. Thus we should commit the message which was chosen for round `r` according to the received quadruple. After replacing our quadruple as described above, we combine it with the quadruple we received. Whether that occurred or not, if the now current round's bit field is full of ones, commit to the current round's maximum message and replace the quadruple as described above.

Whether we began a new round or not, if the quadruple is now different than before receiving the message, we broadcast the new quadruple to every output channel.

Every time we commit to a message, we append it to the list of messages and we add `r + 1` times the value of the message to an accumulator. Once the time limit is reached, we print the list of messages and the accumulated score.


Proof of correctness
--

As I have proved earlier, at most two rounds are in progress at any time. Therefore, when the time limit is reached, each node either considers `r` or `r + 1` to be the current round. This means each node has committed to either `r` or `r + 1` messages before the current round, and so assuming they have committed to the same message on corresponding rounds, the difference between the outputs will be within the desired tolerance of one extra message at the end.

I now need to demonstrate that the committed messages will indeed be the same for corresponding rounds. I shall prove a stronger statement: if we fix the RNG seeds from which each node generates its message candidates, then there is a fixed infinite sequence of messages `ms = [m r | r <- [0..]]` such that any node which commits to a message in round `r` will commit to the message `m r`. That is, while the network conditions affect which prefix of `ms` will be returned by the nodes, the sequence `ms` itself is deterministic.

The proof is straightforward. Since the only place at which nodes use random numbers is when generating candidate messages, with a given seed each node will always generate the same sequence of candidate messages. And since each node always waits until it has committed to a message for the previous round before generating its next candidate, each node `n` will always generate the same candidate `c n r` for round `r`, if any. So on each round, there is a fixed pool of candidates `cs r = [c n r | n <- ns]` from which the committed message is computed. I claim that `m r = maximum cs`.

I now need to prove that if a node does commit to a message on round `r`, that message must be `maximum cs`. Since each node waits until its bitfield is full of ones before committing to a message, and each bit can only become one if the corresponding candidate `c n r` has been combined into the final message, we know that the message committed by node `n` is a combination of at least one copy of each element of `cs`. Since we combine those elements with `max`, the result is `maximum cs`, as desired. Since `max` is associative and commutative, the fact that messages can be delivered out of order has no impact on the result, and since `max` is idempotent, the fact that messages can be delivered more than once had no impact either. The fact that messages might not be delivered does have an impact on whether the node commits to a message or not, but if any candidate is missing, it will not commit to a message, so this again does not have an impact on the committed message of the nodes which do commit.


Fault tolerance
--

In its current form, the algorithm is resistant to network failures which only affect a subset of the channels: if the remaining subset of edges is strongly-connected and messages are always eventually delivered along that subset of the channels, then the information which could not be sent directly will still reach the target node indirectly and the algorithm will continue making progress.

If, on the other hand, there are network failures which change over time, for example if all the channels fail and then later come back up, or if the network becomes temporarily partitioned, then we can get into a state in which no nodes make progress despite all the channels being recruited. In particular, if all the messages in which a particular bit is set are blocked from reaching a single node, then every node will stop progressing, as they will wait for that bit to become one and this will never happen since messages are never re-sent.

We could obviously have every node continuously re-broadcast their most recent quadruple, and that would be quite wasteful, but in the absence of stronger guarantees about the network's reliability, we cannot do much better. So let's create a more accurate model of the possible failures. Since the implementation is based on `Network.Transport.TCP`, which automatically reconnects and re-sends the messages if the interruption is short, or raises a "broken channel" event if the interruption is prolonged, my model is simple: if the channel breaks, I expect to be notified that it did.

Under the circumstances, the only change I need to make to my algorithm is that if a node is notified that its channel to another node is broken, it should repeatedly attempt to reconnect, and upon success, it should send its latest quadruple along the new channel.


Proof of progress
--

With the above change, we are guaranteed something along the lines of eventual consistency, but for progress: if a subset of the channels eventually become stable, meaning that further messages are always eventually delivered along that subset of the channels, and this subset is strongly connected, then the algorithm is guaranteed to progress forever.

Obviously, this statement entails that the algorithm also makes progress when there are no disconnections, so this section is both a regular proof of progress and a proof that the algorithm retains progress under the network conditions described above.

Here is a proof by induction on the round number `r`. I want to show that round `r` eventually completes, meaning that every node commits to a message for round `r`. It suffices to show that an arbitrary node `n` commits to a message for round `r`. By induction, either the previous round eventually completes, or this is the first round. In either case, every node eventually begins working on round `r` by broadcasting candidate `c n r`. More importantly, they each broadcast a bit field in which their corresponding index is set, and if all of those bits reach `n`, then `n` will commit to a message for round `r`. It suffices to show that the bit of an arbitrary node `n'` eventually reaches `n`.

Technically, what we need is a quadruple which is greater than or equal to the candidate sent by node `n'` according to the partial order of the quadruple's semi-lattice: if it is a quadruple for round `r`, its quadruple will include a one at position `n'`, and if it is a quadruple from a later round, it will cause `n` to commit to a message for round `r` if it hasn't already. Since nodes only ever send messages which are greater than all previously-received messages, it suffices to show that there is a path from `n'` to `n` along which a message is sent, it is eventually received, and the receiver later sends another message to the next node in the path and so on. For technical reasons, we shall prove a slightly weaker statement in which the receiver must at some point send a message which is greater than or equal to the message it received, but that messages might not necessarily be sent after receiving the other message. This is still sufficient to show that `n` will eventually receive a message which is greater than or equal to `n'`'s original candidate.

Let's pick the path from `n'` to `n` along the strongly-connected subset which eventually becomes stable. We already know that `n'` eventually broadcasts its candidate when it begins round `r`. In particular, it sends a message along the first edge of our path. Since this channel eventually becomes stable, either the message is eventually received, or the channel breaks and `n'` will eventually send another message along this channel once it becomes stable, at at that point the message is guaranteed to be delivered. So we know that a message eventually reaches the second node along our path, who must then broadcast its combined message towards the third node, and so on. One technical detail is that the second node might not broadcast a message if receiving the message did not cause its quadruple to increase. But in that case, there is still some point in the past at which the node first reached that quadruple, at which point a broadcast did occur.


Optimizing the score
--

Since the goal is to optimize the score under the constraint of being fault tolerant, I would ideally like to prove that my algorithm produces the best possible score under the circumstances. Unfortunately, the very general model of communication I have used for my proof of correctness makes this impossible. Regardless of the chosen algorithm `a` which we would like to prove optimal, we can always find some network conditions under which a different algorithm `a'` performs better than `a`. Pick a very small `ε` and a very large `Ε`, and simply have `a'` sleep for `2*ε` seconds before executing `a`, and then chose adversarial network conditions as follows. We shall choose a set of heavy degradation periods, that is, ranges of time during which network conditions are degraded, causing messages sent during those periods to be delivered `Ε` seconds later than normal. We shall also define a number of light degradation periods during which message delivery is only delayed by a small amount of time. Simply pick the heavy degradation regions to be an interval of size `ε` around all of the times at which `a` tries to send a message. Since `a'` is offset by `2*ε`, its first messages won't be affected, and so most of its future messages should not be affected either because `a'` will execute much faster and will be completely out of sync with `a'`, and since the heavy degradation regions are very small, it is very unlikely that `a'` will try to send a message at that exact time. If it nevertheless happens, simply take the set of messages which caused the node to decide to send a message at this unfortunate time, and add light degradation period around the time at which those messages were sent, thereby causing the node to receive all of those messages a little bit later and thus also to send its message a little bit later, outside of the heavy degradation period. This way, we ensure that all the messages sent by `a'` are received much faster than the messages sent by `a`, thereby ensuring that `a'` progresses much faster.

So I need to pick a much more restricted model of communication. I will assume that sending a message later, multiple times, or with a different content cannot cause that message to arrive earlier nor to be delivered instead of not delivered. One exception is that if the channel subsequently becomes broken, then sending the message after we receive the notification and manage to reconnect may cause the message to be delivered instead of not delivered. These stronger assumptions imply a model in which messages are delivered in the same order in which they are sent, which makes sense for TCP.


Proof of optimality
--

Since the score increases monotonically with the number of messages committed, I will simply attempt to prove that the time it takes to complete each round is minimal. For this, I need a model of how long operations take. Since sending messages across the network is much much slower than performing local computations, I will assume that local computations are instantaneous and that only the latency of sending messages has any impact on the performance.

First, I need to define what I mean by a "round", since competing algorithms might not necessarily be structured in terms of rounds. Since I am only interested in algorithms which satisfy my correctness requirement of having a final output which differs at most by one final message, I can define the notion of "committing" to a message as follows: if node `n` receives no more messages after time `t` and it includes a particular message in its output, then that node is considered to have committed to that message at some point before time `t`. Since the goal is to output as many messages as possible, I will ignore the possibility of degenerate algorithms which would include a message at time `t` but not at a later time `t'`. Then, for a particular node `n`, round `r` begins when `r` messages have been committed so far and ends on the node's next commit. The round begins when the first node begins it, if ever, and ends when the last node ends it, if ever.

Next, I want to define a concept I will call the "future light cone" of an event on a particular node. In physics, this is the the region of space which can be affected by an event occurring at the apex of the cone, it is a visual representation of the fact that objects which are far away from each other cannot influence each other until at least after enough time has passed that a ray of light could travel from one to the other. Similarly, our future light cone will include all the events throughout the system which could possibly be influenced by the event which occurred on a particular node. This includes all the events which occur after the event on the same node, and also all the messages which are sent or could have been during this period, the message reception event in the destination nodes and all their future events, the messages which could have been sent by those nodes during these periods, and so on.

I want to show that (1) a node cannot complete a round before it has entered the future light cone of all the events at which all the other nodes began that round, and (2) with my algorithm, a node completes a round at the exact time at which it enters the last of those future light cones, and therefore my algorithm completes rounds as early as possible and is optimal.

To show (1), consider what happens if node `n` commits to a message for round `r+2` before it has entered the future light cone of some other node `n'`. Clearly, `n` cannot possibly know that `n'` has already committed to a message for round `r+1`, and in fact, we can modify the network conditions to force `n'` not to commit to any message on rounds `r+1` and above, without affecting the behaviour of node `n`. Therefore, the last message in the output of `n'` is that of round `r`, while the last message in the output of `n` is that of round `n+2` or later, they differ by more than one final message, and so this is not a valid algorithm.

To show (2), let's try to visualize at which point node `n` enters the future light cone of the event at which node `n'` began round `r`. A message could be sent at the exact moment at which node `n'` begins round `r`, so the light cone propagates to all the other nodes. And indeed, my algorithm broadcast a message at this exact moment. Furthermore, the network conditions affect the future light cone in the same way they affect my algorithm: if a channel is broken, neither the light cone nor my messages will manage to go through. Also, according to the assumptions of my more restricted model of communication, no message sent at any later point could arrive earlier than those broadcast messages I am sending, so the future light cone reaches the destination nodes at the exact same time at which my algorithm's messages arrive, at which point the destination node performs its own broadcast, and so on.

Node `n` enters the future light cone of node `n'` via the shortest path of messages along non-broken channels from `n'` to `n` via every combination of intermediate nodes. Along that path, the node receiving the quadruple will see bit `n'` set for the first time, so it will update its quadruple and perform a broadcast towards the next node in the shortest path, who will also broadcast, and so on until we reach `n`. Node `n` thus sets its `n'` bit to one, like the others, when it enters the future light cone of `n'`. As soon as node `n` has entered the last future light cone, it discovers that all of the bits are set to one, and it commits, as early as it possibly could.


Incorrect improvements
--

Here are a few improvements which might spring to mind.

Instead of dropping all the messages except the maximum, couldn't we accumulate the messages from all the nodes, sort them by value, and thereby commit to as many messages as there are nodes on each round, say 100, instead of just 1?

Or: instead of using this protocol on every message, couldn't the nodes optimistically exchange a large number of messages, and only perform the costly protocol in order to commit, say each batch of 100 messages?

Both of those strategies have the same flaw: while they do commit to messages faster under ideal network conditions, committing to more than one message at a time means that under adversarial network conditions, some nodes will now output 100 fewer messages than others instead of just one, so these variants do not satisfy my correctness requirements.

Speaking of which, there are also other improvements which might aim at improving the probability that all the nodes have the exact same output, accepting that the one message difference might happen, but only under very unlikely network conditions. I don't think that doing so is a good idea. By making the case in which there is a one message difference very unlikely, others are likely to write code which assumes that this case never happens, which might cause bugs which are difficult to reproduce because the conditions under which they can be reproduced are intentionally unlikely. I think it is better to embrace the limitations of an algorithm and to build a more robust system around it, than to hide its limitations and to accidentally build a fragile system around it.


Limiting bandwidth usage
--

Achieving an optimal score comes at the cost of a considerable amount of bandwidth. Bandwidth usage has no effect on my theoretical model, since the limit of one commit per round means that the latency is the most important factor, but bandwidth does have a significant impact in the real world, and it can even impact latency if messages queue up somewhere in the middle.

The biggest problem of my algorithm is that each message received triggers a broadcast, so several messages are sent, each of which in turn causes several other messages to be sent, and so on exponentially. The exponential growth eventually stops once every node commits to a message and stops propagating the messages from the previous round, and then it begins anew with the next round. If the network conditions restrict the number of messages which can be exchanged in a given period of time, this exponential curve can be quite problematic.

To fix the problem, I would like to bound the bandwidth to a fixed number of in-transit messages. If there are `N` nodes, I will limit my bandwidth usage to `O(N²)` in-transit messages at a time. To do so, instead of broadcasting every improvement, each node sends exactly one message to every other node, and waits for a message from each node before sending that node the next message. Since the other node sent a message as well, the response might come before the question is received, which is fine since the order in which the messages are received is irrelevant for correctness. The result is that each bidirectional channel continuously has two messages bouncing back and forth between the two nodes.

This optimization does not impact the correctness of the algorithm: since we are simply sending fewer messages, this reduces to an execution of the original algorithm in which the network conditions are such that the missing messages don't get delivered. However, there is one subtlety regarding progress: previously, if a node received a contribution which did not improve its quadruple, the node did not perform a broadcast, because it has already broadcasted that information in the past. In this variant, node `n` must continue the ping-pong game with `n'` even if `n` did not learn anything from the message `n'` has sent this time, because `n'` might have some new information to send in the future, and `n` will never receive this new information unless it first sends a message to `n'`.

By sending fewer messages, we of course sacrifice our theoretical optimality result. In the worst case, the two messages travelling back and forth between two nodes end up bouncing in-phase instead of out-of-phase, and the latency is as bad as if there was only one message bouncing back and forth. Instead of propagating the information as fast as the future light cone does, the time it takes for the information to propagate now depends on the position of the bouncing messages when the information arrives at each node. It could be that each time a piece of information arrives, it arrives immediately after all the messages have just left, so we have to wait for an entire round trip, and then a third trip for the information to travel with the message to the next node. This could happen on every node along every path, so compared to a single trip by the future light cone, in the worst case this variant can be up to three times slower than the optimal variant.


Extra features
--

1. In addition to the required flags `--send-for` and `--wait-for`, and the suggested flag `--with-seed`, I have also implemented a `--verbosity` flag. The log levels are: 0 to only display the output at the end, 1 to display messages about established and lost connections, plus a few one-time messages, 2 to also display each committed message (I recommend turning this on, disabling the internet and then turning it back on to see the messages pause and resume), 3 to see every message sent and received, and 4 to also see the internal state changes every time the quadruple improves. There is no default verbosity level, it is a required flag.
2. I have also implemented `--omit-message-list`, because the list of messages can get very long, causing an unwieldy output at the end. In this flag is turned on, I recommend bumping the verbosity to 1 or above in order to also see the off-by-one-message scores, otherwise it can be confusing to see the different nodes output different scores. If the flag is turned off, I recomment redirecting the output to a file so that the time it takes to print all of those messages to a terminal does not cause my program to go beyond the grace period.
3. In my implementation, I have used a free monad to separate the implementation of the base algorithm from the its interpreter, which handles terminating the algorithm on time, accumulating and printing the committed messages, and the fault-tolerance and constant-bandwidth improvements discussed afterwards. This would make it easy to compare different algorithms, assuming the aforementioned improvements apply to those algorithms as well, and also to compare different interpreters, for example one which does not perform some of those improvements.


Possible improvements
--

1. In addition to the current interpreter, I was planning to implement a pure interpreter. Simulating message transmissions would make it easy to model different network conditions in order to demonstrate that the algorithm is robust.
2. Since the algorithm and the interpreter are very event-driven, I wonder if it would be possible to rewrite them in an FRP style, and whether that would make things clearer than the current imperative style?
3. Instead of keeping exactly two messages per bidirectional channel at all times, each node could keep a counter for each channel in order to keep `N` messages per bidirectional channel at all times, that might reduce the performance gap between the optimal and constant-number-of-in-transit-messages variants.

Configuration
--

The file `node_list.txt` must contain the network-process addresses of all the nodes, something like this:

    192.168.0.123:8080:0
    192.168.0.123:8081:0
    192.168.0.123:8082:0
    192.168.0.102:8083:0
    192.168.0.102:8084:0
    192.168.0.102:8085:0
