package engine

import (
	"context"
	"errors"
)

// based on: https://www.complang.tuwien.ac.at/ulrich/iso-prolog/dcgs/dcgsdin150408.pdf

// Phrase succeeds if the difference list of s0-s satisfies the grammar rule of grBody.
func (state *State) Phrase(grBody, s0, s Term, k func(*Env) *Promise, env *Env) *Promise {
	goal, err := dcgBody(grBody, s0, s, env)
	if err != nil {
		return Error(err)
	}
	return Delay(func(context.Context) *Promise {
		return state.Call(goal, k, env)
	})
}

var errDCGNotApplicable = errors.New("not applicable")

func expandDCG(term Term, env *Env) (Term, error) {
	rule, ok := env.Resolve(term).(*Compound)
	if !ok || rule.Functor != "-->" || len(rule.Args) != 2 {
		return nil, errDCGNotApplicable
	}

	s0, s1, s := NewVariable(), NewVariable(), NewVariable()
	if c, ok := env.Resolve(rule.Args[0]).(*Compound); ok && c.Functor == "," && len(c.Args) == 2 {
		head, err := dcgNonTerminal(c.Args[0], s0, s, env)
		if err != nil {
			return nil, err
		}
		goal1, err := dcgBody(rule.Args[1], s0, s1, env)
		if err != nil {
			return nil, err
		}
		goal2, err := dcgTerminals(c.Args[1], s, s1, env)
		if err != nil {
			return nil, err
		}
		body := Atom(",").Apply(goal1, goal2)
		return Atom(":-").Apply(head, body), nil
	}

	head, err := dcgNonTerminal(rule.Args[0], s0, s, env)
	if err != nil {
		return nil, err
	}
	body, err := dcgBody(rule.Args[1], s0, s, env)
	if err != nil {
		return nil, err
	}
	return Atom(":-").Apply(head, body), nil
}

func dcgNonTerminal(nonTerminal, list, rest Term, env *Env) (Term, error) {
	pi, args, err := piArgs(nonTerminal, env)
	if err != nil {
		return nil, err
	}
	return pi.Name.Apply(append(args, list, rest)...), nil
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
	return Atom("=").Apply(list, ListRest(rest, elems...)), nil
}

var dcgConstr map[ProcedureIndicator]func(args []Term, list, rest Term, env *Env) (Term, error)

func init() {
	dcgConstr = map[ProcedureIndicator]func(args []Term, list, rest Term, env *Env) (Term, error){
		{Name: "[]", Arity: 0}: func(_ []Term, list, rest Term, _ *Env) (Term, error) {
			return Atom("=").Apply(list, rest), nil
		},
		{Name: ".", Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return dcgTerminals(Atom(".").Apply(args...), list, rest, env)
		},
		{Name: ",", Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			v := NewVariable()
			first, err := dcgBody(args[0], list, v, env)
			if err != nil {
				return nil, err
			}
			second, err := dcgBody(args[1], v, rest, env)
			if err != nil {
				return nil, err
			}
			return Atom(",").Apply(first, second), nil
		},
		{Name: ";", Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			body := dcgBody
			if t, ok := env.Resolve(args[0]).(*Compound); ok && t.Functor == "->" && len(t.Args) == 2 {
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
			return Atom(";").Apply(either, or), nil
		},
		{Name: "|", Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			either, err := dcgBody(args[0], list, rest, env)
			if err != nil {
				return nil, err
			}
			or, err := dcgBody(args[1], list, rest, env)
			if err != nil {
				return nil, err
			}
			return Atom(";").Apply(either, or), nil
		},
		{Name: "{}", Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return Atom(",").Apply(args[0], Atom("=").Apply(list, rest)), nil
		},
		{Name: "call", Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return Atom("call").Apply(args[0], list, rest), nil
		},
		{Name: "phrase", Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			return Atom("phrase").Apply(args[0], list, rest), nil
		},
		{Name: "!", Arity: 0}: func(_ []Term, list, rest Term, env *Env) (Term, error) {
			return Atom(",").Apply(Atom("!"), Atom("=").Apply(list, rest)), nil
		},
		{Name: `\+`, Arity: 1}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			v := NewVariable()
			g, err := dcgBody(args[0], list, v, env)
			if err != nil {
				return nil, err
			}
			return Atom(",").Apply(Atom(`\+`).Apply(g), Atom("=").Apply(list, rest)), nil
		},
		{Name: "->", Arity: 2}: func(args []Term, list, rest Term, env *Env) (Term, error) {
			v := NewVariable()
			cond, err := dcgBody(args[0], list, v, env)
			if err != nil {
				return nil, err
			}
			then, err := dcgBody(args[1], v, rest, env)
			if err != nil {
				return nil, err
			}
			return Atom("->").Apply(cond, then), nil
		},
	}
}

func dcgBody(term, list, rest Term, env *Env) (Term, error) {
	term = env.Resolve(term)
	if t, ok := term.(Variable); ok {
		return Atom("phrase").Apply(t, list, rest), nil
	}

	t, err := dcgCBody(term, list, rest, env)
	if errors.Is(err, errDCGNotApplicable) {
		return dcgNonTerminal(term, list, rest, env)
	}
	return t, err
}

func dcgCBody(term, list, rest Term, env *Env) (Term, error) {
	pi, args, err := piArgs(term, env)
	if err != nil {
		return nil, err
	}
	if c, ok := dcgConstr[pi]; ok {
		return c(args, list, rest, env)
	}
	return nil, errDCGNotApplicable
}
