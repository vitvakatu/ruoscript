// Modified from pest project
// Copyright (c) 2018 Dragoș Tiselice
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::BitOr;

use pest::iterators::Pair;
use pest::RuleType;

/// An `enum` describing an `Operator`'s associativity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Assoc {
    /// Left `Operator` associativity
    Left,
    /// Right `Operator` associativity
    Right,
}

#[derive(Debug)]
pub enum OperatorType {
    Infix { assoc: Assoc },
    Prefix,
}

#[derive(Debug)]
pub struct Operator<R: RuleType> {
    op_type: OperatorType,
    rule: R,
    next: Option<Box<Operator<R>>>,
}

impl<R: RuleType> Operator<R> {
    pub fn new_infix(rule: R, assoc: Assoc) -> Operator<R> {
        Operator {
            op_type: OperatorType::Infix { assoc },
            rule,
            next: None,
        }
    }

    pub fn new_prefix(rule: R) -> Operator<R> {
        Operator {
            op_type: OperatorType::Prefix,
            rule,
            next: None,
        }
    }
}

impl<R: RuleType> BitOr for Operator<R> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self {
        fn assign_next<R: RuleType>(op: &mut Operator<R>, next: Operator<R>) {
            if let Some(ref mut child) = op.next {
                assign_next(child, next);
            } else {
                op.next = Some(Box::new(next));
            }
        }

        assign_next(&mut self, rhs);
        self
    }
}

#[derive(Debug)]
pub struct PrecClimber<R: RuleType> {
    ops: HashMap<R, (u32, Option<Assoc>)>,
}

impl<R: RuleType> PrecClimber<R> {
    pub fn new(ops: Vec<Operator<R>>) -> PrecClimber<R> {
        let ops = ops
            .into_iter()
            .zip(1..)
            .fold(HashMap::new(), |mut map, (op, precedence)| {
                let mut next = Some(op);

                while let Some(op) = next.take() {
                    let assoc = if let OperatorType::Infix { assoc } = op.op_type { Some(assoc) } else { None };
                    map.insert(op.rule, (precedence, assoc));
                    next = op.next.map(|op| *op);
                }

                map
            });

        PrecClimber { ops }
    }

    pub fn climb<'i, P, F, G, T>(&self, mut pairs: P, mut primary: F, mut infix: G) -> T
    where
        P: Iterator<Item = Pair<'i, R>>,
        F: FnMut(Pair<'i, R>) -> T,
        G: FnMut(T, Pair<'i, R>, Option<T>) -> T,
    {
        let lhs = primary(
            pairs
                .next()
                .expect("precedence climbing requires a non-empty Pairs"),
        );
        self.climb_rec(lhs, 0, &mut pairs.peekable(), &mut primary, &mut infix)
    }

    fn climb_rec<'i, P, F, G, T>(
        &self,
        mut lhs: T,
        min_precedence: u32,
        pairs: &mut Peekable<P>,
        primary: &mut F,
        infix: &mut G,
    ) -> T
    where
        P: Iterator<Item = Pair<'i, R>>,
        F: FnMut(Pair<'i, R>) -> T,
        G: FnMut(T, Pair<'i, R>, Option<T>) -> T,
    {
        while pairs.peek().is_some() {
            let rule = pairs.peek().unwrap().as_rule();
            if let Some(&(precedence, _)) = self.ops.get(&rule) {
                if precedence >= min_precedence {
                    let op = pairs.next().unwrap();
                    let mut rhs = primary(pairs.next().expect(
                        "infix operator must be followed by \
                         a primary expression",
                    ));

                    while pairs.peek().is_some() {
                        let rule = pairs.peek().unwrap().as_rule();
                        if let Some(&(new_precedence, assoc)) = self.ops.get(&rule) {
                            if new_precedence > precedence
                                || assoc == Assoc::Right && new_precedence == precedence
                            {
                                rhs = self.climb_rec(rhs, new_precedence, pairs, primary, infix);
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    lhs = infix(lhs, op, rhs);
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        lhs
    }
}
