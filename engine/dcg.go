package engine

import (
	"context"
	"errors"
)

// based on: https://www.complang.tuwien.ac.at/ulrich/iso-prolog/dcgs/dcgsdin150408.pdf

// Phrase succeeds if the difference list of s0-s satisfies the grammar rule of grBody.
func (vm *VM) Phrase(grBody, s0, s Term, k func(*Env) *Promise, env *Env) *Promise {
	goal, err := dcgBody(grBody, s0, s, env)
	if err != nil {
		return Error(err)
	}
	return Delay(func(context.Context) *Promise {
		return vm.Call(goal, k, env)
	})
}

var errDCGNotApplicable = errors.New("not applicable")

func expandDCG(term Term, env *Env) (Term, error) {
	rule, ok := env.Resolve(term).(Compound)
	if !ok || rule.Functor() != atomArrow || rule.Arity() != 2 {
		return nil, errDCGNotApplicable
	}

	s0, s1, s := NewVariable(), NewVariable(), NewVariable()
	if c, ok := env.Resolve(rule.Arg(0)).(Compound); ok && c.Functor() == atomComma && c.Arity() == 2 {
		head, err := dcgNonTerminal(c.Arg(0), s0, s, env)
		if err != nil {
			return nil, err
		}
		goal1, err := dcgBody(rule.Arg(1), s0, s1, env)
		if err != nil {
			return nil, err
		}
		goal2, err := dcgTerminals(c.Arg(1), s, s1, env)
		if err != nil {
			return nil, err
		}
		body := atomComma.Apply(goal1, goal2)
		return atomIf.Apply(head, body), nil
	}

	head, err := dcgNonTerminal(rule.Arg(0), s0, s, env)
	if err != nil {
		return nil, err
	}
	body, err := dcgBody(rule.Arg(1), s0, s, env)
	if err != nil {
		return nil, err
	}
	return atomIf.Apply(head, body), nil
}

func dcgNonTerminal(nonTerminal, list, rest Term, env *Env) (Term, error) {
	pi, arg, err := PI(nonTerminal, env)
	if err != nil {
		return nil, err
	}
	args := make([]Term, pi.Arity, pi.Arity+2)
	for i := 0; i < int(pi.Arity); i++ {
		args[i] = arg(i)
	}
	args = append(args, list, rest)
	return pi.Name.Apply(args...), nil
}

func dcgTerminals(terminals, list, rest Term, env *Env) (Term, error) {
	var elems []Term
	iter := ListIterator{List: terminals, Env: env}
	for iter.Next() {
		elems = append(elems, iter.Current())
	}
	if err := iter.Err(); err != nil {
		return nil, err
	}
	return atomEqual.Apply(list, ListRest(rest, elems...)), nil
}

var dcgConstr map[ProcedureIndicator]func(args []Term, list, rest Term, env *Env) (Term, error)

func init() {
	dcgConstr = map[ProcedureIndicator]func(args []Term, list, rest Term, env *Env) (Term, error){
		{Name: atomEmptyList, Arity: 0}: func(_ []Term, list, rest Term, _ *Env) (Term, error) {
			return atomEqual.Apply(list, rest), nil
		},
		{Name: atomDot, Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return dcgTerminals(atomDot.Apply(args...), list, rest, env)
		},
		{Name: atomComma, Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			v := NewVariable()
			first, err := dcgBody(args[0], list, v, env)
			if err != nil {
				return nil, err
			}
			second, err := dcgBody(args[1], v, rest, env)
			if err != nil {
				return nil, err
			}
			return atomComma.Apply(first, second), nil
		},
		{Name: atomSemiColon, Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			body := dcgBody
			if t, ok := env.Resolve(args[0]).(Compound); ok && t.Functor() == atomThen && t.Arity() == 2 {
				body = dcgCBody
			}
			either, err := body(args[0], list, rest, env)
			if err != nil {
				return nil, err
			}
			or, err := dcgBody(args[1], list, rest, env)
			if err != nil {
				return nil, err
			}
			return atomSemiColon.Apply(either, or), nil
		},
		{Name: atomBar, Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			either, err := dcgBody(args[0], list, rest, env)
			if err != nil {
				return nil, err
			}
			or, err := dcgBody(args[1], list, rest, env)
			if err != nil {
				return nil, err
			}
			return atomSemiColon.Apply(either, or), nil
		},
		{Name: atomEmptyBlock, Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return atomComma.Apply(args[0], atomEqual.Apply(list, rest)), nil
		},
		{Name: atomCall, Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return atomCall.Apply(args[0], list, rest), nil
		},
		{Name: atomPhrase, Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return atomPhrase.Apply(args[0], list, rest), nil
		},
		{Name: atomCut, Arity: 0}: func(_ []Term, list, rest Term, env *Env) (Term, error) {
			return atomComma.Apply(atomCut, atomEqual.Apply(list, rest)), nil
		},
		{Name: atomNegation, Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			v := NewVariable()
			g, err := dcgBody(args[0], list, v, env)
			if err != nil {
				return nil, err
			}
			return atomComma.Apply(atomNegation.Apply(g), atomEqual.Apply(list, rest)), nil
		},
		{Name: atomThen, Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			v := NewVariable()
			cond, err := dcgBody(args[0], list, v, env)
			if err != nil {
				return nil, err
			}
			then, err := dcgBody(args[1], v, rest, env)
			if err != nil {
				return nil, err
			}
			return atomThen.Apply(cond, then), nil
		},
	}
}

func dcgBody(term, list, rest Term, env *Env) (Term, error) {
	term = env.Resolve(term)
	if t, ok := term.(Variable); ok {
		return atomPhrase.Apply(t, list, rest), nil
	}

	t, err := dcgCBody(term, list, rest, env)
	if errors.Is(err, errDCGNotApplicable) {
		return dcgNonTerminal(term, list, rest, env)
	}
	return t, err
}

func dcgCBody(term, list, rest Term, env *Env) (Term, error) {
	pi, arg, err := PI(term, env)
	if err != nil {
		return nil, err
	}
	if c, ok := dcgConstr[pi]; ok {
		args := make([]Term, pi.Arity)
		for i := 0; i < int(pi.Arity); i++ {
			args[i] = arg(i)
		}
		return c(args, list, rest, env)
	}
	return nil, errDCGNotApplicable
}
