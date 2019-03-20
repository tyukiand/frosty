use crate::process::Proc;
use crate::process::ProcCons::*;
use crate::process::ValueCons::*;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::iter::Iterator;
use uuid::Uuid;

/// Two parallel `VecDeque`s, one holding receivers, the other holding senders.
struct CommChannel {
    receivers: VecDeque<Proc>,
    senders: VecDeque<Proc>,
}

impl CommChannel {
    fn new() -> Self {
        CommChannel {
            receivers: VecDeque::new(),
            senders: VecDeque::new(),
        }
    }

    /// Assumes a `Sender` or `Receiver`, pushes it into the corresponding
    /// deque. Does not verify the name.
    fn push_back(&mut self, p: Proc) -> () {
        match p {
            Receive { .. } => self.receivers.push_back(p),
            Tell { .. } => self.senders.push_back(p),
            _ => {
                panic!(
                    "Fatal error: Attempted to push non-Receive and \
                     non-Tell proc into CommChannel: {:?}",
                    p
                );
            }
        }
    }

    /// Attempts to deque a receiver and a sender simultaneously.
    fn pop_front(&mut self) -> Option<(Proc, Proc)> {
        if self.receivers.is_empty() || self.senders.is_empty() {
            None
        } else {
            let r = self
                .receivers
                .pop_front()
                .expect("A receiver must be dequed from a non-empty VecDeque");
            let s = self
                .senders
                .pop_front()
                .expect("A receiver must be dequed from a non-empty VecDeque");
            Some((r, s))
        }
    }

    /// Dismantles both deques,
    /// collects the remaining processes into a single vector and returns it.
    fn harvest(self) -> impl Iterator<Item = Proc> {
        self.receivers.into_iter().chain(self.senders.into_iter())
    }
}

pub struct CommQueue {
    rcv_snd_queue: VecDeque<(Proc, Proc)>,
    uuid_channels: HashMap<Uuid, CommChannel>,
    surrogate_channels: HashMap<u64, CommChannel>,
}

impl CommQueue {
    pub fn new() -> Self {
        CommQueue {
            rcv_snd_queue: VecDeque::new(),
            uuid_channels: HashMap::new(),
            surrogate_channels: HashMap::new(),
        }
    }

    /// Enqueues a process that wants to communicate on a channel (`Receive` or
    /// `Tell`).
    ///
    /// Sets up a new channel queue if necessary. If there is a matching
    /// complementary process in an already existing channel, the two processes
    /// are paired and moved into a queue of pairs, which then can be extracted
    /// using `pop_front`.
    pub fn push_back(&mut self, p: Proc) -> () {
        let chan_name = match p.peek_channel() {
            Some(n) => n,
            None => panic!(
              "Every process pushed into CommQueue must have \
               a channel on which it wants to communicate. \
               Attempted to push: {:?}", p
            )
        };
        match chan_name {
            UnforgeableName(uuid) => {
                if !self.uuid_channels.contains_key(uuid) {
                    self.uuid_channels.insert(uuid.clone(), CommChannel::new());
                }
                let chan = self.uuid_channels.get_mut(uuid).unwrap();
                chan.push_back(p);
                for pair in chan.pop_front() {
                    self.rcv_snd_queue.push_back(pair);
                }
            }
            PathSurrogate(n) => {
                if !self.surrogate_channels.contains_key(n) {
                    self.surrogate_channels.insert(*n, CommChannel::new());
                }
                let chan = self.surrogate_channels.get_mut(n).unwrap();
                chan.push_back(p);
                for pair in chan.pop_front() {
                    self.rcv_snd_queue.push_back(pair);
                }
            }
            BuiltInChannelName(bicn) => {
                let receiver_process = BuiltInService(*bicn);
                self.rcv_snd_queue.push_back((receiver_process, p));
            }
            _ => panic!(
                "Unexpected name (neither UUID nor local surrogate) arrived \
                 at the 'CommQueue`: {:?}. \
                 That's a bug in the VM implementation, someone probably \
                 forgot to handle one of the special built-in channels.",
                chan_name
            ),
        };
    }

    /// Attempts to deque a pair of processes on which the COMM-rule can
    /// be applied. The first one is always the receiver, the second one
    /// is always the sender.
    pub fn pop_front(&mut self) -> Option<(Proc, Proc)> {
        self.rcv_snd_queue.pop_front()
    }

    /// Dismantles all internal structures of this queue, collects all
    /// processes in all channels into a single vector and returns it.
    pub fn harvest(self) -> Vec<Proc> {
        let mut result: Vec<Proc> = Vec::new();
        for (r, s) in self.rcv_snd_queue.into_iter() {
            result.push(r);
            result.push(s);
        }

        for (_name, chan) in self.uuid_channels.into_iter() {
            result.extend(chan.harvest());
        }
        for (_name, chan) in self.surrogate_channels.into_iter() {
            result.extend(chan.harvest());
        }
        result
    }
}
