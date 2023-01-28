package engine

import (
	"context"
	"errors"
)

// based on: https://www.complang.tuwien.ac.at/ulrich/iso-prolog/dcgs/dcgsdin150408.pdf

// Phrase succeeds if the difference list of s0-s satisfies the grammar rule of grBody.
func Phrase(ctx context.Context, grBody, s0, s Term) *Promise {
	goal, err := dcgBody(ctx, grBody, s0, s)
	if err != nil {
		return Error(err)
	}
	return Call(ctx, goal)
}

var errDCGNotApplicable = errors.New("not applicable")

func expandDCG(ctx context.Context, term Term) (Term, error) {
	rule, ok := Resolve(ctx, term).(Compound)
	if !ok || rule.Functor() != atomArrow || rule.Arity() != 2 {
		return nil, errDCGNotApplicable
	}

	s0, s1, s := NewVariable(), NewVariable(), NewVariable()
	if c, ok := Resolve(ctx, rule.Arg(0)).(Compound); ok && c.Functor() == atomComma && c.Arity() == 2 {
		head, err := dcgNonTerminal(ctx, c.Arg(0), s0, s)
		if err != nil {
			return nil, err
		}
		goal1, err := dcgBody(ctx, rule.Arg(1), s0, s1)
		if err != nil {
			return nil, err
		}
		goal2, err := dcgTerminals(ctx, c.Arg(1), s, s1)
		if err != nil {
			return nil, err
		}
		body := atomComma.Apply(goal1, goal2)
		return atomIf.Apply(head, body), nil
	}

	head, err := dcgNonTerminal(ctx, rule.Arg(0), s0, s)
	if err != nil {
		return nil, err
	}
	body, err := dcgBody(ctx, rule.Arg(1), s0, s)
	if err != nil {
		return nil, err
	}
	return atomIf.Apply(head, body), nil
}

func dcgNonTerminal(ctx context.Context, nonTerminal, list, rest Term) (Term, error) {
	pi, arg, err := piArg(ctx, nonTerminal)
	if err != nil {
		return nil, err
	}
	args := make([]Term, pi.arity, pi.arity+2)
	for i := 0; i < int(pi.arity); i++ {
		args[i] = arg(i)
	}
	args = append(args, list, rest)
	return pi.name.Apply(args...), nil
}

func dcgTerminals(ctx context.Context, terminals, list, rest Term) (Term, error) {
	var elems []Term
	iter := ListIterator{List: terminals}
	for iter.Next(ctx) {
		elems = append(elems, iter.Current())
	}
	if err := iter.Err(); err != nil {
		return nil, err
	}
	return atomEqual.Apply(list, PartialList(rest, elems...)), nil
}

var dcgConstr map[procedureIndicator]func(ctx context.Context, args []Term, list, rest Term) (Term, error)

func init() {
	dcgConstr = map[procedureIndicator]func(ctx context.Context, args []Term, list, rest Term) (Term, error){
		{name: atomEmptyList, arity: 0}: func(_ context.Context, _ []Term, list, rest Term) (Term, error) {
			return atomEqual.Apply(list, rest), nil
		},
		{name: atomDot, arity: 2}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			return dcgTerminals(ctx, atomDot.Apply(args...), list, rest)
		},
		{name: atomComma, arity: 2}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			v := NewVariable()
			first, err := dcgBody(ctx, args[0], list, v)
			if err != nil {
				return nil, err
			}
			second, err := dcgBody(ctx, args[1], v, rest)
			if err != nil {
				return nil, err
			}
			return atomComma.Apply(first, second), nil
		},
		{name: atomSemiColon, arity: 2}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			body := dcgBody
			if t, ok := Resolve(ctx, args[0]).(Compound); ok && t.Functor() == atomThen && t.Arity() == 2 {
				body = dcgCBody
			}
			either, err := body(ctx, args[0], list, rest)
			if err != nil {
				return nil, err
			}
			or, err := dcgBody(ctx, args[1], list, rest)
			if err != nil {
				return nil, err
			}
			return atomSemiColon.Apply(either, or), nil
		},
		{name: atomBar, arity: 2}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			either, err := dcgBody(ctx, args[0], list, rest)
			if err != nil {
				return nil, err
			}
			or, err := dcgBody(ctx, args[1], list, rest)
			if err != nil {
				return nil, err
			}
			return atomSemiColon.Apply(either, or), nil
		},
		{name: atomEmptyBlock, arity: 1}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			return atomComma.Apply(args[0], atomEqual.Apply(list, rest)), nil
		},
		{name: atomCall, arity: 1}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			return atomCall.Apply(args[0], list, rest), nil
		},
		{name: atomPhrase, arity: 1}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			return atomPhrase.Apply(args[0], list, rest), nil
		},
		{name: atomCut, arity: 0}: func(ctx context.Context, _ []Term, list, rest Term) (Term, error) {
			return atomComma.Apply(atomCut, atomEqual.Apply(list, rest)), nil
		},
		{name: atomNegation, arity: 1}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			v := NewVariable()
			g, err := dcgBody(ctx, args[0], list, v)
			if err != nil {
				return nil, err
			}
			return atomComma.Apply(atomNegation.Apply(g), atomEqual.Apply(list, rest)), nil
		},
		{name: atomThen, arity: 2}: func(ctx context.Context, args []Term, list, rest Term) (Term, error) {
			v := NewVariable()
			cond, err := dcgBody(ctx, args[0], list, v)
			if err != nil {
				return nil, err
			}
			then, err := dcgBody(ctx, args[1], v, rest)
			if err != nil {
				return nil, err
			}
			return atomThen.Apply(cond, then), nil
		},
	}
}

func dcgBody(ctx context.Context, term, list, rest Term) (Term, error) {
	term = Resolve(ctx, term)
	if t, ok := term.(Variable); ok {
		return atomPhrase.Apply(t, list, rest), nil
	}

	t, err := dcgCBody(ctx, term, list, rest)
	if errors.Is(err, errDCGNotApplicable) {
		return dcgNonTerminal(ctx, term, list, rest)
	}
	return t, err
}

func dcgCBody(ctx context.Context, term, list, rest Term) (Term, error) {
	pi, arg, err := piArg(ctx, term)
	if err != nil {
		return nil, err
	}
	if c, ok := dcgConstr[pi]; ok {
		args := make([]Term, pi.arity)
		for i := 0; i < int(pi.arity); i++ {
			args[i] = arg(i)
		}
		return c(ctx, args, list, rest)
	}
	return nil, errDCGNotApplicable
}
