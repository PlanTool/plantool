#! /usr/bin/env python
# -*- coding: latin-1 -*-

#######################################################################
#
# Author: Malte Helmert (helmert@informatik.uni-freiburg.de)
# (C) Copyright 2003-2004 Malte Helmert
# Modified by: Gabi Roeger and Silvia Richter (silvia.richter@nicta.com.au)
# (C) Copyright 2008: Gabi Roeger and NICTA
#
# This file is part of LAMA.
#
# LAMA is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the license, or (at your option) any later version.
#
# LAMA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
#
#######################################################################

import copy

import pddl

class ConditionProxy(object):
  def clone_owner(self):
    clone = copy.copy(self)
    clone.owner = copy.copy(clone.owner)
    return clone
    
class PreconditionProxy(ConditionProxy):
  def __init__(self, action):
    self.owner = action
    self.condition = action.precondition
  def set(self, new_condition):
    self.owner.precondition = self.condition = new_condition
  def register_owner(self, task):
    task.actions.append(self.owner)
  def delete_owner(self, task):
    task.actions.remove(self.owner)
  def build_rules(self, rules):
    action = self.owner
    rule_head = get_action_predicate(action)
    rule_body = list(condition_to_rule_body(action.parameters,
                                            self.condition))
    rules.append((rule_body, rule_head))
  def get_type_map(self):
    return self.owner.type_map

class EffectConditionProxy(ConditionProxy):
  def __init__(self, action, effect):
    self.action = action
    self.owner = effect
    self.condition = effect.condition
  def set(self, new_condition):
    self.owner.condition = self.condition = new_condition
  def register_owner(self, task):
    self.action.effects.append(self.owner)
  def delete_owner(self, task):
    self.action.effects.remove(self.owner)
  def build_rules(self, rules):
    effect = self.owner
    rule_head = effect.literal
    if not rule_head.negated:
      rule_body = [get_action_predicate(self.action)]
      rule_body += condition_to_rule_body([], self.condition)
      rules.append((rule_body, rule_head))
  def get_type_map(self):
    return self.action.type_map

class AxiomConditionProxy(ConditionProxy):
  def __init__(self, axiom):
    self.owner = axiom
    self.condition = axiom.condition
  def set(self, new_condition):
    self.owner.condition = self.condition = new_condition
  def register_owner(self, task):
    task.axioms.append(self.owner)
  def delete_owner(self, task):
    task.axioms.remove(self.owner)
  def build_rules(self, rules):
    axiom = self.owner
    app_rule_head = get_axiom_predicate(axiom)
    app_rule_body = list(condition_to_rule_body(axiom.parameters, self.condition))
    rules.append((app_rule_body, app_rule_head))
    eff_rule_head = pddl.Atom(axiom.name, [par.name for par in axiom.parameters])
    eff_rule_body = [app_rule_head]
    rules.append((eff_rule_body, eff_rule_head))
  def get_type_map(self):
    return self.owner.type_map

class GoalConditionProxy(ConditionProxy):
  def __init__(self, task):
    self.owner = task
    self.condition = task.goal
  def set(self, new_condition):
    self.owner.goal = self.condition = new_condition
  def build_rules(self, rules):
    rule_head_name = "@goal-reachable"
    rule_head = pddl.Atom("@goal-reachable", [])
    rule_body = list(condition_to_rule_body([], self.condition))
    rules.append((rule_body, rule_head))
  def get_type_map(self):
    # HACK!
    # Method uniquify_variables HAS already been called (which is good).
    # We call it here again for its SIDE EFFECT of collecting the type_map
    # (which is bad). Having "top-level conditions" (currently, only goal
    # conditions, but might also include safety conditions and similar)
    # contained in a separate wrapper class that stores a type map might
    # be a better design.
    type_map = {}
    self.condition.uniquify_variables(type_map)
    return type_map

def get_action_predicate(action):
  name = action
  variables = [par.name for par in action.parameters]
  if isinstance(action.precondition, pddl.ExistentialCondition):
    variables += [par.name for par in action.precondition.parameters]
  return pddl.Atom(name, variables)

def get_axiom_predicate(axiom):
  name = axiom
  variables = [par.name for par in axiom.parameters]
  if isinstance(axiom.condition, pddl.ExistentialCondition):
    variables += [par.name for par in axiom.condition.parameters]
  return pddl.Atom(name, variables)

def all_conditions(task):
  for action in task.actions:
    yield PreconditionProxy(action)
    for effect in action.effects:
      yield EffectConditionProxy(action, effect)
  for axiom in task.axioms:
    yield AxiomConditionProxy(axiom)
  yield GoalConditionProxy(task)

# [1] Remove universal quantifications from conditions.
#
# Replace, in a top-down fashion, <forall(vars, phi)> by <not(not-all-phi)>,
# where <not-all-phi> is a new axiom.
#
# <not-all-phi> is defined as <not(forall(vars,phi))>, which is of course
# translated to NNF. The parameters of the new axioms are exactly the free
# variables of <forall(vars, phi)>.

def remove_universal_quantifiers(task):
  def recurse(condition):
    # Uses new_axioms_by_condition and type_map from surrounding scope.
    if isinstance(condition, pddl.UniversalCondition):
      axiom_condition = condition.negate()
      parameters = axiom_condition.free_variables()
      axiom = new_axioms_by_condition.get(axiom_condition)
      if not axiom:
        typed_parameters = [pddl.TypedObject(v, type_map[v]) for v in parameters]
        condition = recurse(axiom_condition)
        axiom = task.add_axiom(typed_parameters, condition)
        new_axioms_by_condition[condition] = axiom
      return pddl.NegatedAtom(axiom.name, parameters)
    else:
      new_parts = [recurse(part) for part in condition.parts]
      return condition.change_parts(new_parts)

  new_axioms_by_condition = {}
  for proxy in tuple(all_conditions(task)):
    # Cannot use generator because we add new axioms on the fly.
    if proxy.condition.has_universal_part():
      type_map = proxy.get_type_map()
      proxy.set(recurse(proxy.condition))

    
# [2] Pull disjunctions to the root of the condition.
#
# After removing universal quantifiers, the (k-ary generalization of the)
# following rules suffice for doing that: 
# (1) or(phi, or(psi, psi'))      ==  or(phi, psi, psi')
# (2) exists(vars, or(phi, psi))  ==  or(exists(vars, phi), exists(vars, psi))
# (3) and(phi, or(psi, psi'))     ==  or(and(phi, psi), and(phi, psi'))
def build_DNF(task):
  def recurse(condition):
    disjunctive_parts = []
    other_parts = []
    for part in condition.parts:
      part = recurse(part)
      if isinstance(part, pddl.Disjunction):
        disjunctive_parts.append(part)
      else:
        other_parts.append(part)
    if not disjunctive_parts:
      return condition

    # Rule (1): Associativity of disjunction.
    if isinstance(condition, pddl.Disjunction):
      result_parts = other_parts
      for part in disjunctive_parts:
        result_parts.extend(part.parts)
      return pddl.Disjunction(result_parts)

    # Rule (2): Distributivity disjunction/existential quantification.
    if isinstance(condition, pddl.ExistentialCondition):
      parameters = condition.parameters
      result_parts = [pddl.ExistentialCondition(parameters, (part,))
                      for part in disjunctive_parts[0].parts]
      return pddl.Disjunction(result_parts)

    # Rule (3): Distributivity disjunction/conjunction.
    assert isinstance(condition, pddl.Conjunction)
    result_parts = [pddl.Conjunction(other_parts)]
    while disjunctive_parts:
      previous_result_parts = result_parts
      result_parts = []
      parts_to_distribute = disjunctive_parts.pop().parts
      for part1 in previous_result_parts:
        for part2 in parts_to_distribute:
          result_parts.append(pddl.Conjunction((part1, part2)))
    return pddl.Disjunction(result_parts)

  for proxy in all_conditions(task):
    if proxy.condition.has_disjunction():
      proxy.set(recurse(proxy.condition).simplified())

# [3] Split conditions at the outermost disjunction.
def split_disjunctions(task):
  for proxy in tuple(all_conditions(task)):
    # Cannot use generator directly because we add/delete entries.
    if isinstance(proxy.condition, pddl.Disjunction):
      for part in proxy.condition.parts:
        new_proxy = proxy.clone_owner()
        new_proxy.set(part)
        new_proxy.register_owner(task)
      proxy.delete_owner(task)

# [4] Pull existential quantifiers out of conjunctions and group them.
#
# After removing universal quantifiers and creating the disjunctive form,
# only the following (representatives of) rules are needed:
# (1) exists(vars, exists(vars', phi))  ==  exists(vars + vars', phi)
# (2) and(phi, exists(vars, psi))       ==  exists(vars, and(phi, psi)),
#       if var does not occur in phi as a free variable.
def move_existential_quantifiers(task):
  def recurse(condition):
    existential_parts = []
    other_parts = []
    for part in condition.parts:
      part = recurse(part)
      if isinstance(part, pddl.ExistentialCondition):
        existential_parts.append(part)
      else:
        other_parts.append(part)
    if not existential_parts:
      return condition

    # Rule (1): Combine nested quantifiers.
    if isinstance(condition, pddl.ExistentialCondition):
      new_parameters = condition.parameters + existential_parts[0].parameters
      new_parts = existential_parts[0].parts
      return pddl.ExistentialCondition(new_parameters, new_parts)

    # Rule (2): Pull quantifiers out of conjunctions.
    assert isinstance(condition, pddl.Conjunction)
    new_parameters = []
    new_conjunction_parts = other_parts
    for part in existential_parts:
      new_parameters += part.parameters
      new_conjunction_parts += part.parts
    new_conjunction = pddl.Conjunction(new_conjunction_parts)
    return pddl.ExistentialCondition(new_parameters, (new_conjunction,))

  for proxy in all_conditions(task):
    if proxy.condition.has_existential_part():
      proxy.set(recurse(proxy.condition).simplified())

def substitute_complicated_goal(task):
  goal = task.goal
  if isinstance(goal, pddl.Literal):
    return
  elif isinstance(goal, pddl.Conjunction):
    simple_goal = True
    for item in goal.parts:
      if not isinstance(item, pddl.Literal):
        simple_goal = False
        break
    if simple_goal:
      return
  new_axiom = task.add_axiom([], goal)
  task.goal = pddl.Atom(new_axiom.name, new_axiom.parameters)

# Combine Steps [1], [2], [3], [4]
def normalize(task):
  remove_universal_quantifiers(task)
  substitute_complicated_goal(task)
  build_DNF(task)
  split_disjunctions(task)
  move_existential_quantifiers(task)

# [5] Build rules for exploration component.
def build_exploration_rules(task):
  result = []
  for proxy in all_conditions(task):
    proxy.build_rules(result)
  return result

def condition_to_rule_body(parameters, condition):
  for par in parameters:
    yield pddl.Atom(par.type, [par.name])
  if not isinstance(condition, pddl.Truth):
    if isinstance(condition, pddl.ExistentialCondition):
      for par in condition.parameters:
        yield pddl.Atom(par.type, [par.name])
      condition = condition.parts[0]
    if isinstance(condition, pddl.Conjunction):
      parts = condition.parts
    else:
      parts = (condition,)
    for part in parts:
      assert isinstance(part, pddl.Literal), "Condition not normalized"
      if not part.negated:
        yield part

if __name__ == "__main__":
  task = pddl.open()
  normalize(task)
  task.dump()
