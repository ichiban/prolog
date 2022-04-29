package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNew(t *testing.T) {
	t.Run("call_nth", func(t *testing.T) {
		var s struct {
			N   int
			Nth int
		}

		p := New(nil, nil)

		assert.NoError(t, p.QuerySolution(`call_nth(true, Nth), Nth = 1.`).Err())

		sols, err := p.Query(`call_nth(repeat, Nth).`)
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 1, s.Nth)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 2, s.Nth)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 3, s.Nth)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 4, s.Nth)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 5, s.Nth)
		assert.NoError(t, sols.Close())

		sols, err = p.Query(`call_nth(( N = 1 ; N = 2 ), Nth).`)
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 1, s.N)
		assert.Equal(t, 1, s.Nth)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 2, s.N)
		assert.Equal(t, 2, s.Nth)
		assert.NoError(t, sols.Close())

		assert.NoError(t, p.QuerySolution(`catch(call_nth(true, non_integer), error(type_error(integer,non_integer), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(call_nth(true, 1.0), error(type_error(integer,1.0), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`\+call_nth(true, 0).`).Err())
		assert.NoError(t, p.QuerySolution(`\+call_nth(repeat, 0).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(call_nth(repeat, -1), error(domain_error(not_less_than_zero,-1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`call_nth(length(L,N), 3), L = [_A,_B], N = 2.`).Err())
		assert.NoError(t, p.QuerySolution(`\+call_nth(inex, 0).`).Err())
		assert.NoError(t, p.QuerySolution(`\+call_nth(1, 0).`).Err())
		assert.NoError(t, p.QuerySolution(`\+call_nth(V, 0).`).Err())
	})
}
