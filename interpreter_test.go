package prolog

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"github.com/ichiban/prolog/engine"
	"github.com/stretchr/testify/assert"
	"io"
	"os"
	"regexp"
	"testing"
	"time"
)

func TestNew(t *testing.T) {
	i := New(nil, nil)
	assert.NotNil(t, i)

	t.Run("number_chars", func(t *testing.T) {
		// http://www.complang.tuwien.ac.at/ulrich/iso-prolog/number_chars
		p := New(nil, nil)

		// Section 0
		assert.NoError(t, p.QuerySolution(`number_chars(1.2,['1',.,'2']).`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(1.0e9,['1',.,'0','E','9']).`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(1,['0','1']).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[a]), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[]), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[[]]), error(type_error(character,[]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[' ',[]]), error(type_error(character,[]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[0]), error(type_error(character,0), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[_,[]]), error(type_error(character,[]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[X]), error(instantiation_error,_), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['0'|_]), error(instantiation_error,_), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,'1'), error(type_error(list,'1'), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[a|a]), error(type_error(list,[a|a]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[49]), error(type_error(character,49), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[]), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['3',' ']), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['3',.]), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(N,[' ','1']), N = 1.`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(N,['\n','1']), N = 1.`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(N,[' ','0','''',a]), N = 97.`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(N,[-,' ','1']), N = -1.`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(N,[/,*,*,/,'1']), N = 1.`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(N,['%','\n','1']), N = 1.`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[-,/,*,*,/,'1']), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['1',e,'1']), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['1',.,'0',e]), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['1',.,'0',e,e]), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(N,['0',x,'1']), N = 1.`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['0','X','1']), error(syntax_error(_), _), true).`).Err())
		assert.Error(t, p.QuerySolution(`catch(number_chars(1E1,_), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`\+number_chars(1,['.'|_]).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[+,'1']), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[+,' ','1']), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['''',+,'''','1']), error(syntax_error(_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['11']), error(type_error(character,_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,['1.1']), error(type_error(character,_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1+1,['2']), error(type_error(number,1+1), _), true).`).Err())

		// Section 2
		assert.NoError(t, p.QuerySolution(`number_chars(1,[C]), C = '1'.`).Err())
		assert.NoError(t, p.QuerySolution(`\+number_chars(1,[C,D]).`).Err())
		assert.NoError(t, p.QuerySolution(`\+number_chars(1,[C,C]).`).Err())
		assert.NoError(t, p.QuerySolution(`\+number_chars(0,[C,C]).`).Err())
		assert.NoError(t, p.QuerySolution(`number_chars(10,[C,D]), C = '1', D = '0'.`).Err())
		assert.NoError(t, p.QuerySolution(`\+number_chars(100,[C,D]).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[X|2]), error(instantiation_error,_), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(N,[1|_]), error(type_error(character,1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(V,[1|2]), error(type_error(character,1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars([],1), error(type_error(list,1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,1), error(type_error(list,1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[a|2]), error(type_error(list,[a|2]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[_|2]), error(type_error(list,[_|2]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[[]|_]), error(type_error(character,[]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(number_chars(1,[[]|2]), error(type_error(character,[]), _), true).`).Err())

		assert.NoError(t, p.QuerySolution(`catch((L=['1'|L], number_chars(N,L)), error(type_error(list,['1'|_]), _), L=['1'|L]).`).Err())
	})

	t.Run("length", func(t *testing.T) {
		// http://www.complang.tuwien.ac.at/ulrich/iso-prolog/length_quad.pl
		p := New(nil, nil)

		var s struct {
			L []engine.Term
			N int
		}

		sols, err := p.Query(`length(L,N).`)
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Len(t, s.L, 0)
		assert.Equal(t, 0, s.N)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Len(t, s.L, 1)
		assert.Equal(t, 1, s.N)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Len(t, s.L, 2)
		assert.Equal(t, 2, s.N)
		assert.NoError(t, sols.Close())

		assert.NoError(t, p.QuerySolution(`length(L,0), L = [].`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`length([_|L],0).`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`length(2,0).`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`length([_|2],0).`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`length([_|2],N).`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`length([_|2],2).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length(L,-1), error(domain_error(not_less_than_zero,-1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length([],-1), error(domain_error(not_less_than_zero,-1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length(a,-1), error(domain_error(not_less_than_zero,-1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length([],-0.1), error(type_error(integer,-0.1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length(L,-0.1), error(type_error(integer,-0.1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length([a],1.0), error(type_error(integer,1.0), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length(L,1.0), error(type_error(integer,1.0), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length(L,1.1), error(type_error(integer,1.1), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length(L,1.0e99), error(type_error(integer,1.0e99), _), true).`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`N is 2^52, length([], N).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length([],0+0), error(type_error(integer,0+0), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length([],-_), error(type_error(integer,-_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length([a],-_), error(type_error(integer,-_), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length([a,b|X],X), error(resource_error(finite_memory), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch(length(L,L), error(resource_error(finite_memory), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch((L = [_|_], length(L,L)), error(type_error(integer,[_|_]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch((L = [_], length(L,L)), error(type_error(integer,[_]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch((L = [1], length(L,L)), error(type_error(integer,[1]), _), true).`).Err())
		assert.NoError(t, p.QuerySolution(`catch((L = [a|L], length(L,N)), error(resource_error(finite_memory), _), true).`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`L = [a|L], length(L,0).`).Err())
		assert.Equal(t, ErrNoSolutions, p.QuerySolution(`L = [a|L], length(L,7).`).Err())
		/* This library doesn't implement freeze/2
		assert.NoError(t, p.QuerySolution(`freeze(L,L=[]), \+length(L,L).`).Err())
		ctx, cancel = context.WithTimeout(context.Background(), 100*time.Millisecond)
		assert.Equal(t, context.Canceled, p.QuerySolutionContext(ctx, `freeze(L,L=[_|L]), length(L,N).`).Err())
		cancel()
		assert.NoError(t, p.QuerySolution(`freeze(L,L=[_|L]), N is 2^64, \+length(L,N).`).Err())
		*/

		sols, err = p.Query(`length([a, b|L], N).`)
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Len(t, s.L, 0)
		assert.Equal(t, 2, s.N)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Len(t, s.L, 1)
		assert.Equal(t, 3, s.N)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Len(t, s.L, 2)
		assert.Equal(t, 4, s.N)
		assert.NoError(t, sols.Close())
	})
}

func TestNew_variableNames(t *testing.T) {
	// http://www.complang.tuwien.ac.at/ulrich/iso-prolog/variable_names
	// I wanted to put this under TestNew() as t.Run("variable_names", ...) but GoLand didn't recognize it as a table-driven test.

	var out bytes.Buffer
	p := New(nil, &out)

	defer func() {
		_ = os.Remove("f") // Some test cases open a file 'f'.
	}()

	tests := []struct {
		name     string
		input    string
		query    string
		output   string
		outputFn func(t *testing.T, output string)
		err      error
	}{
		{name: "1", query: `catch(write_term(T,[quoted(true), variable_names([N=T])]), error(instantiation_error, _), true).`},
		{name: "2", query: `N = 'X', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `X`},
		{name: "3", query: `catch((N = T, write_term(T,[quoted(true), variable_names([N=T])])), error(instantiation_error, _), true).`},
		{name: "4", query: `N = '_', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `_`},
		{name: "65", query: `N = '_/*.*/', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `_/*.*/`},
		{name: "5", query: `N = x, write_term(T,[quoted(true), variable_names([N=T])]).`, output: `x`},
		{name: "6", query: `N = 'x+y', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `x+y`},
		{name: "50", query: `N = '))', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `))`},
		{name: "7", query: `catch((N = 7, write_term(T,[quoted(true), variable_names([N=T])])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "8", query: `catch((N = 1+2, write_term(T,[quoted(true), variable_names([N=T])])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "9", query: `catch((N = '$VAR'(9), write_term(T,[quoted(true), variable_names([N=T])])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "10", query: `catch((T = a, write_term(T,[quoted(true), variable_names([N=T])])), error(instantiation_error, _), true).`},
		{name: "11", query: `T = a, N = 'Any', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `a`},
		{name: "12", query: `T = '$VAR'(9), N = '_', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `'$VAR'(9)`},
		// {name: "28", query: `freeze(T,throw(g(T))), N = 'X', write_term(T,[quoted(true), variable_names([N=T])]).`, output: `'$VAR'(9)`}, This implementation doesn't support freeze/2.
		{name: "13", query: `write_term(T,[quoted(true), variable_names(['X'=X,'Y'=Y,'Z'=Z])]).`, outputFn: func(t *testing.T, output string) {
			t.Helper()
			assert.Regexp(t, regexp.MustCompile(`\A_\d+\z`), output)
		}},
		{name: "14", query: `T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['X'=X,'Y'=Y,'Z'=Z])]).`, output: `X,Y,Z`},
		{name: "15", query: `Z=Y, T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['X'=X,'Y'=Y,'Z'=Z])]).`, output: `X,Y,Y`},
		{name: "16", query: `Z=Y, Y=X, T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['X'=X,'Y'=Y,'Z'=Z])]).`, output: `X,X,X`},
		{name: "17", query: `T=(Y,Z), write_term(T,[quoted(true), variable_names(['X'=X,'Y'=Y,'Z'=Z])]).`, output: `Y,Z`},
		{name: "18", query: `T=(Z,Y), write_term(T,[quoted(true), variable_names(['X'=X,'Y'=Y,'Z'=Z])]).`, output: `Z,Y`},
		{name: "19", query: `write_term(T,[quoted(true), variable_names(['Z'=Z,'Y'=Y,'X'=X])]).`, outputFn: func(t *testing.T, output string) {
			t.Helper()
			assert.Regexp(t, regexp.MustCompile(`\A_\d+\z`), output)
		}},
		{name: "20", query: `T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['Z'=Z,'Y'=Y,'X'=X])]).`, output: `X,Y,Z`},
		{name: "21", query: `Z=Y, T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['Z'=Z,'Y'=Y,'X'=X])]).`, output: `X,Z,Z`},
		{name: "22", query: `Z=Y, Y=X, T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['Z'=Z,'Y'=Y,'X'=X])]).`, output: `Z,Z,Z`},
		{name: "23", query: `T=(Y,Z), write_term(T,[quoted(true), variable_names(['Z'=Z,'Y'=Y,'X'=X])]).`, output: `Y,Z`},
		{name: "24", query: `T=(Z,Y), write_term(T,[quoted(true), variable_names(['Z'=Z,'Y'=Y,'X'=X])]).`, output: `Z,Y`},
		{name: "25", query: `write_term(T,[quoted(true), variable_names(['X'=Z,'X'=Y,'X'=X])]).`, outputFn: func(t *testing.T, output string) {
			t.Helper()
			assert.Regexp(t, regexp.MustCompile(`\A_\d+\z`), output)
		}},
		{name: "26", query: `T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['X'=Z,'X'=Y,'X'=X])]).`, output: `X,X,X`},
		{name: "27", query: `T=(1,2,3), T=(X,Y,Z), write_term(T,[quoted(true), variable_names(['X'=Z,'X'=Y,'X'=X])]).`, output: `1,2,3`},
		{name: "32", query: `read_term(T,[variable_names(VN_list)]), VN_list=[_=1,_=2,_=3], writeq(VN_list).`, err: context.DeadlineExceeded},
		{name: "29", input: `B+C+A+B+C+A.`, query: `read_term(T,[variable_names(VN_list)]), VN_list=[_=1,_=2,_=3], writeq(VN_list).`, output: `['B'=1,'C'=2,'A'=3]`},
		{name: "30", query: `catch(write_term(T, [variable_names(VN_list)]), error(instantiation_error, _), true).`},
		{name: "31", query: `catch((VN_list = 1, write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "33", query: `catch((VN_list = [[]], write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "34", query: `catch((VN_list = non_list, write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "35", query: `catch((VN_list = [T='T'|non_list], write_term(T, [variable_names(VN_list)])), error(instantiation_error, _), true).`},
		{name: "52", query: `catch((VN_list = ['T'=T|_], write_term(T, [variable_names(VN_list)])), error(instantiation_error, _), true).`},
		{name: "51", query: `catch((VN_list = ['T'=T|non_list], write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "36", query: `catch((VN_list = [T-'T'], write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "63", query: `catch((VN_list = [_,a], write_term(T, [variable_names(VN_list)])), error(instantiation_error, _), true).`},
		{name: "64", query: `catch((VN_list = [a,_], write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "66", query: `catch((VN_list = [a|_], write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names(_)), _), true).`},
		{name: "67", query: `catch((VN_list = [i=i,7=i], write_term(T, [variable_names(VN_list)])), error(domain_error(write_option, variable_names([i=i,7=i])), _), true).`},
		{name: "68", query: `catch((VN_list = [_,_], write_term(T, [variable_names(VN_list)])), error(instantiation_error, _), true).`},
		{name: "43", query: `write_term(-X^2,[variable_names(['X'=X])]).`, output: `- (X^2)`},
		{name: "44", query: `X=1, write_term(-X^2,[variable_names(['X'=X])]).`, output: `- (1^2)`},
		{name: "37", query: `catch(open(f,write,_,[O]), error(instantiation_error, _), true).`},
		{name: "38", query: `catch((O = 1, open(f,write,_,[O])), error(domain_error(stream_option, 1), _), true).`},
		{name: "56", query: `catch((O = typex(_), open(f,write,_,[O])), error(domain_error(stream_option, typex(_)), _), true).`},
		{name: "57", query: `catch((O = typex(1), open(f,write,_,[O])), error(domain_error(stream_option, typex(1)), _), true).`},
		{name: "62", query: `catch((O = typex(s(_)), open(f,write,_,[O])), error(domain_error(stream_option, typex(s(_))), _), true).`},
		{name: "39", query: `O = type(text), open(f,write,_,[O]).`},
		{name: "40", query: `catch((O = type(1), open(f,write,_,[O])), error(domain_error(stream_option, type(1)), _), true).`},
		{name: "41", query: `catch((O = type(_), open(f,write,_,[O])), error(instantiation_error, _), true).`},
		{name: "60", query: `catch((O = alias(_), open(f,write,_,[O])), error(instantiation_error, _), true).`},
		{name: "42", query: `catch((O = type(nontype), open(f,write,_,[O])), error(domain_error(stream_option, type(nontype)), _), true).`},
		{name: "61", query: `catch((O = alias(1), open(f,write,_,[O])), error(domain_error(stream_option, alias(1)), _), true).`},
		{name: "45", query: `read_term(T,[variable_names(VN_list)]).`, err: context.DeadlineExceeded},
		{name: "46", input: `a.`, query: `read_term(T,[variable_names(VN_list)]), T = a, VN_list = [].`},
		{name: "47", query: `VN_list = 42, read_term(T,[variable_names(VN_list)]).`, err: context.DeadlineExceeded},
		{name: "48", input: `a.`, query: `VN_list = 42, \+read_term(T,[variable_names(VN_list)]).`},
		{name: "49", input: `a b.`, query: `VN_list = 42, catch(read_term(T,[variable_names(VN_list)]), error(syntax_error(_), _), true).`},
		{name: "53", query: `catch(write_term(S,[quoted(true), variable_names([N=T])]), error(instantiation_error, _), true).`},
		{name: "54", query: `S=1+T,N='/*r*/V',write_term(S,[quoted(true), variable_names([N=T])]).`, output: `1+/*r*/V`},
		{name: "55", query: `S=1+T,N=' /*r*/V',write_term(S,[quoted(true), variable_names([N=T])]).`, output: `1+ /*r*/V`},
		{name: "58", query: `S=1+T,N=(+),write_term(S,[quoted(true), variable_names([N=T])]).`, output: `1++`},
		{name: "59", query: `S=T+1,N=(+),write_term(S,[quoted(true), variable_names([N=T])]).`, output: `++1`},
		{name: "69", query: `read_term(T, [singletons(1)]).`, err: context.DeadlineExceeded},
		{name: "70", input: `a.`, query: `\+read_term(T, [singletons(1)]).`},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
			defer cancel()

			if tt.input == "" {
				p.SetUserInput(engine.NewInputTextStream(readFn(func(p []byte) (n int, err error) {
					<-ctx.Done()
					return 0, io.EOF
				})))
			} else {
				p.SetUserInput(engine.NewInputTextStream(bytes.NewBufferString(tt.input)))
			}
			out.Reset()
			assert.Equal(t, tt.err, p.QuerySolutionContext(ctx, tt.query).Err())
			if tt.outputFn == nil {
				assert.Equal(t, tt.output, out.String())
			} else {
				tt.outputFn(t, out.String())
			}
		})
	}
}

func TestNew_conformity(t *testing.T) {
	// http://www.complang.tuwien.ac.at/ulrich/iso-prolog/conformity_testing

	tests := []struct {
		name     string
		premise  string
		input    string
		output   string
		err      error
		outputFn func(t *testing.T, output string)
	}{
		{name: "1", input: `writeq('\n').`, output: `'\n'`},
		{name: "2", input: `'`, output: `syntax err.`},
		{name: "3", input: `)`, output: `syntax err.`},
		{name: "261", input: ")\n'", output: `syntax err.`},
		{name: "4", input: `.`, output: `syntax err.`},
		{name: "5", input: `writeq('	'). % horiz. tab`, output: `syntax err.`},
		{name: "177", input: `0'\t=0'	. % horiz. tab`, output: `syntax err.`},
		{name: "6", input: `writeq('
').`, output: `syntax err.`},
		{name: "7", input: `writeq('\
'). % "\\\n"`, output: `''`},
		{name: "8", input: `writeq('\
a'). % "\\\na"`, output: `a`},
		{name: "8", input: `writeq('\
a'). % "\\\na"`, output: `a`},
		{name: "9", input: `writeq('a\
b'). % "a\\\nb"`, output: `ab`},
		{name: "10", input: `writeq('a\
 b'). % "a\\\n b"`, output: `'a b'`},
		{name: "11", input: `writeq('\ ').`, output: `syntax err.`},
		{name: "193", input: `writeq('\ 
'). % "\\ \n"`, output: `syntax err.`},
		{name: "12", input: `writeq('\	'). % "\\\t"`, output: `syntax err.`},
		{name: "13", input: `writeq('\t').`, output: `'\t'`},
		{name: "14", input: `writeq('\a').`, output: `'\a'`},
		{name: "15", input: `writeq('\7\').`, output: `'\a'`},
		{name: "16", input: `writeq('\ca').`, output: `syntax err.`},
		{name: "241", input: `writeq('\d').`, output: `syntax err.`},
		{name: "17", input: `writeq('\e').`, output: `syntax err.`},
		{name: "18", input: `writeq('\033\').`, output: `'\x1b\'`},
		{name: "301", input: `writeq('\0\').`, output: `'\x0\'`},
		{name: "19", input: `char_code('\e',C).`, output: `syntax err.`},
		{name: "21", input: `char_code('\d',C).`, output: `syntax err.`},
		{name: "22", input: `writeq('\u1').`, output: `syntax err.`},
		{name: "23", input: `X = 0'\u1.`, output: `syntax err.`},
		{name: "24", input: `writeq('`, output: `syntax err.`},
		{name: "25", input: `writeq(.`, output: `syntax err.`},
		{name: "26", input: `'\
''.`, output: `syntax err.`},
		{name: "210", input: `X = 0'\.`, output: `syntax err.`},
		{name: "211", input: `X = 0'\. .`, output: `syntax err.`},
		{name: "222", input: `writeq((-)-(-)).`, output: `(-)-(-)`},
		{name: "223", input: `writeq(((:-):-(:-))).`, output: `(:-):-(:-)`},
		{name: "27", input: `writeq((*)=(*)).`, output: `(*)=(*)`},
		{name: "28", input: `writeq([:-,-]).`, output: `[:-,-]`},
		{name: "29", input: `writeq(f(*)).`, output: `f(*)`},
		{name: "30", input: `writeq(a*(b+c)).`, output: `a*(b+c)`},
		{name: "31", input: `writeq(f(;,'|',';;')).`, output: `f(;,'|',';;')`},
		{name: "32", input: `writeq([.,.(.,.,.)]).`, output: `['.','.'('.','.','.')]`},
		{name: "33", input: `writeq((a :- b,c)).`, output: `a:-b,c`},
		{name: "34", input: `write_canonical([a]).`, output: `'.'(a,[])`},
		{name: "35", input: `writeq('/*').`, output: `'/*'`},
		{name: "203", input: `writeq(//*).`, output: `//*`},
		{name: "282", input: `writeq(//*.*/).`, output: `//*.*/`},
		{name: "36", input: `writeq('/**').`, output: `'/**'`},
		{name: "37", input: `writeq('*/').`, output: `*/`},
		{name: "38", input: "\"\\'\\`\\\"\" = \"'`\"\"\". % \""},               // "\'\`\"" = "'`""". % "
		{name: "179", input: "\"\\'\\\"\" = \"'\"\"\". % \""},                  // "\'\"" = "'""". % "
		{name: "178", input: "\"\\`\" = \"`\"."},                               // "\`" = "`".
		{name: "39", input: "'\\'\\`\\\"' = '''`\\\"'."},                       // '\'\`\"' = '''`"'.
		{name: "40", input: "writeq('\\'\\`\\\"\\\"').", output: "'\\'`\"\"'"}, // writeq('\'\`\"\"'). ==> '\'`""'
		{name: "41", input: `('\\') = (\).`},
		{name: "42", premise: `op(1,xf,xf1).`, input: `1xf1 = xf1(1).`},
		{name: "43", input: `X = 0X1.`, output: `syntax err.`},
		{name: "44", input: `float(.0).`, output: `syntax err.`},
		{name: "45", premise: `op(100,xfx,.).`, input: `functor(3 .2,F,A), F = ('.'), A = 2.`},
		{name: "46", input: `float(- .0).`, output: `syntax err.`},
		{name: "47", input: `float(1E9).`, output: `syntax err.`},
		{name: "48", input: `integer(1e).`, output: `syntax err.`},
		{name: "49", premise: `op(9,xf,e9).`, input: `1e9 = e9(1).`},
		{name: "50", premise: `op(9,xf,e).`, input: `1e-9 = -(e(1),9).`},
		{name: "51", premise: `op(9,xf,e).`, input: `1.0e- 9 = -(e(1.0),9).`},
		{name: "204", premise: `op(9,xf,e).`, input: `writeq(1e).`, output: `1 e`},
		{name: "220", premise: `op(9,xf,e).`, input: `writeq(1.0e).`, output: `1.0 e`},
		{name: "52", premise: `op(9,xfy,e).`, input: `1.2e 3 = e(X,Y), X = 1.2, Y = 3.`},
		{name: "53", input: `writeq(1.0e100).`, output: `1.0e+100`},
		{name: "54", input: `float(1.0ee9).`, output: `syntax err.`},
		{name: "286", input: `(- (1)) = -(1).`},
		{name: "287", input: `(- -1) = -(-1).`},
		{name: "288", input: `(- 1^2) = ^(-1,2).`},
		{name: "56", input: `integer(- 1).`},
		{name: "57", input: `integer('-'1).`},
		{name: "58", input: `integer('-' 1).`},
		{name: "59", input: `integer(- /*.*/1).`},
		{name: "60", input: `integer(-/*.*/1).`, output: `syntax err.`},
		{name: "61", input: `integer('-'/*.*/1).`},
		{name: "62", input: `atom(-/*.*/-).`},
		{name: "63", input: `op(0,fy,-).`},
		{name: "180", premise: `op(0,fy,-).`, input: `integer(-1).`},
		{name: "64", premise: `op(0,fy,-).`, input: `integer(- 1).`},
		{name: "135", input: `writeq(-(1)).`, output: `- (1)`},
		{name: "136", input: `op(0,fy,-),writeq(-(1)).`, output: `-(1)`},
		{name: "182", input: `writeq(-(-1)).`, output: `- -1`},
		{name: "183", input: `writeq(-(1^2)).`, output: `- (1^2)`},
		{name: "260", input: `writeq(-(a^2)).`, output: `- (a^2)`},
		{name: "139", input: `writeq(-((a,b))).`, output: `- (a,b)`},
		{name: "218", input: `writeq(-(1*2)).`, output: `- (1*2)`},
		{name: "140", input: `writeq(-a).`, output: `-a`},
		{name: "184", input: `writeq(-(-)).`, output: `- (-)`},
		{name: "185", input: `writeq(-[-]).`, output: `-[-]`},
		{name: "188", input: `writeq(-p(c)).`, output: `-p(c)`},
		{name: "189", input: `writeq(-{}).`, output: `-{}`},
		{name: "190", input: `writeq(-{a}).`, output: `-{a}`},
		{name: "191", input: `writeq(-(-a)).`, output: `- -a`},
		{name: "192", input: `writeq(-(-(-a))).`, output: `- - -a`},
		{name: "216", input: `writeq(-(-(1))).`, output: `- - (1)`},
		{name: "215", premise: `op(100,yfx,~).`, input: `writeq(-(1~2~3)).`, output: `- (1~2~3)`},
		{name: "248", premise: `op(100,yfx,~).`, input: `writeq(- (1~2)).`, output: `- (1~2)`},
		{name: "249", premise: `op(100,yfx,~).`, input: `writeq(1~2).`, output: `1~2`},
		{name: "278", input: `op(9,xfy,.), writeq(-[1]).`, output: `-[1]`},
		{name: "279", input: `op(9,xf,'$VAR'), writeq(- '$VAR'(0)).`, output: `-A`},
		{name: "296", premise: `op(9,xf,'$VAR').`, input: `writeq('$VAR'(0)).`, output: `A`},
		{name: "55", premise: `op(1,yf,yf1).`, input: `{-1 yf1}={yf1(X)}, X = -1.`},
		{name: "65", input: `compound(+1).`},
		{name: "66", input: `compound(+ 1).`},
		{name: "277", input: `writeq(+1^2).`, output: `+1^2`},
		{name: "67", premise: `op(0,fy,+).`, input: `compound(+1).`, output: `syntax err.`},
		{name: "257", input: `writeq([+{a},+[]]).`, output: `[+{a},+[]]`},
		{name: "68", input: `[(:-)|(:-)]=[:-|:-].`},
		{name: "69", input: `X=[a|b,c].`, output: `syntax err.`},
		{name: "70", input: `op(1000,xfy,',').`, output: `permission_error(modify,operator,',')`},
		{name: "71", input: `op(1001,xfy,',').`, output: `permission_error(modify,operator,',')`},
		{name: "72", input: `op(999,xfy,'|').`, output: `permission_error(modify,operator,'|')`},
		{name: "73", input: `X=[a|b].`},
		{name: "285", premise: `op(0,xfy,'|').`, input: `X=[(a|b)].`, output: `syntax err.`},
		{name: "219", input: `[a|[]]=[a].`},
		{name: "74", input: `X=[a|b|c].`, output: `syntax err.`},
		{name: "75", input: `var(a:-b).`, output: `syntax err.`},
		{name: "76", input: `:- = :- .`, output: `syntax err.`},
		{name: "77", input: `- = - .`, output: `syntax err.`},
		{name: "78", input: `* = * .`, output: `syntax err.`},
		{name: "79", input: `current_op(200,fy,-).`},
		{name: "80", input: `current_op(200,fy,+).`},
		{name: "81", input: `{- - c}={-(-(c))}.`},
		{name: "82", input: `(- -) = -(-).`, output: `syntax err.`},
		{name: "83", input: `(- - -) = -(-(-)).`, output: `syntax err.`},
		{name: "84", input: `(- - - -) = -(-(-(-))).`, output: `syntax err.`},
		{name: "85", input: `{:- :- c} = {:-(:-,c)}.`, output: `syntax err.`},
		{name: "86", input: `{- = - 1}={(-(=)) - 1}.`, output: `syntax err.`},
		{name: "87", input: `write_canonical((- = - 1)).`, output: `syntax err.`},
		{name: "88", input: `write_canonical((- = -1)).`, output: `syntax err.`},
		{name: "89", input: `write_canonical((-;)).`, output: `syntax err.`},
		{name: "90", input: `write_canonical((-;-)).`, output: `syntax err.`},
		{name: "91", input: `write_canonical((:-;-)).`, output: `syntax err.`},
		{name: "92", input: `[:- -c] = [(:- -c)].`, output: `syntax err.`},
		{name: "93", input: `writeq([a,b|,]).`, output: `syntax err.`},
		{name: "94", input: `X ={,}.`, output: `syntax err.`},
		{name: "95", input: `{1} = {}(1).`},
		{name: "96", input: `write_canonical({1}).`, output: `{}(1)`},
		{name: "97", input: `'[]'(1) = [ ](X), X = 1.`},
		{name: "98", input: `X = [] (1).`, output: `syntax err.`},
		{name: "99", input: `op(100,yfy,op).`, output: `domain_error(operator_specifier,yfy)`},
		{name: "100", input: `'''' = '\''.`},
		{name: "101", input: `a = '\141\'.`},
		{name: "102", input: `a = '\141'.`, output: `syntax err.`},
		{name: "103", input: `X = '\141\141', X = a141.`},
		{name: "104", input: `X = '\9'.`, output: `syntax err.`},
		{name: "105", input: `X = '\N'.`, output: `syntax err.`},
		{name: "106", input: `X = '\\' .`, output: `syntax err.`},
		{name: "107", input: `X = '\77777777777\'.`, output: `syntax err.`},
		{name: "108", input: `a = '\x61\'.`},
		{name: "109", input: `atom_codes('\xG\',Cs).`, output: `syntax err.`},
		{name: "110", input: `atom_codes('\xG1\',Cs).`, output: `syntax err.`},
		{name: "111", input: "atom(`).", output: `syntax err.`},
		{name: "112", input: "atom(`+).", output: `syntax err.`},
		{name: "297", input: "atom(`\n`).", output: `syntax err.`},
		{name: "113", input: "X = `a`.", output: `syntax err.`},
		{name: "114", input: `integer(0'\').`},
		{name: "115", input: `integer(0''').`},
		{name: "116", input: `0''' = 0'\'.`},
		{name: "117", input: `integer(0'').`, output: `syntax err.`},
		{name: "195", input: `op(100,xf,'').`},
		{name: "205", premise: `op(100,xf,'').`, input: `(0 '') = ''(X), X = 0.`},
		{name: "196", premise: `op(100,xf,'').`, input: `writeq(0 '').`, output: `0 ''`},
		{name: "197", premise: `op(100,xf,'').`, input: `writeq(0'').`, output: `0 ''`},
		{name: "118", input: `op(100,xfx,'').`},
		{name: "119", premise: `op(100,xfx,'').`, input: `functor(0 ''1, F, A), F = (''), A = 2.`},
		{name: "120", premise: `op(100,xfx,'').`, input: `functor(0''1, F, A), F = (''), A = 2.`},
		{name: "206", premise: `op(100,xf,f).`, input: `writeq(0'f').`, output: `syntax err.`},
		{name: "207", premise: `op(100,xf,f).`, input: `writeq(0'f'f').`, output: `102 f`},
		{name: "209", premise: `op(100,xf,f).`, input: `writeq(0'ff).`, output: `102 f`},
		{name: "256", premise: `op(100,xf,f).`, input: `writeq(0f).`, output: `0 f`},
		{name: "208", premise: `op(100,xf,'f ').`, input: `writeq(0 'f ').`, output: `0 'f '`},
		{name: "121", input: `X = 2'1.`, output: `syntax err.`},
		{name: "122", premise: `op(100,xfx,'1').`, input: `functor(2'1'y, F, A), F = ('1'), A = 2.`},
		{name: "262", premise: `op(100,xfx,'1').`, input: `functor(2 '1'y, F, A), F = ('1'), A = 2.`},
		{name: "123", input: `X =0'\x41\ , X = 65.`},
		{name: "124", input: `X =0'\x41\, X = 65.`},
		{name: "125", input: `X =0'\x1\, X = 1.`},
		{name: "127", input: `X is 16'mod'2, X = 0.`},
		{name: "128", input: `X is 37'mod'2, X = 1.`},
		{name: "129", input: `X is 0'mod'1.`, output: `syntax err.`},
		{name: "130", input: `X is 1'+'1, X = 2.`},
		{name: "212", input: `X is 1'\
+'1, X = 2.`},
		{name: "213", input: `X is 0'\
+'1, X = 1.`},
		{name: "259", input: `X = 0'\
+'/*'. %*/1, X = 0+1.`},
		{name: "303", input: `X = 0'\
a.`, output: `syntax err.`},
		{name: "214", input: `X is 0'\`, output: `syntax err.`}, // TODO: waits
		{name: "126", input: `X = 0'\
.\`, output: `syntax err.`}, // TODO: waits
		{name: "131", input: `op(100,fx,' op').`},
		{name: "132", premise: `op(100,fx,' op').`, input: `writeq(' op' '1').`, output: `' op' '1'`},
		{name: "133", premise: `op(100,fx,' op').`, input: `writeq(' op'[]).`, output: `' op'[]`},
		{name: "134", premise: `op(1,xf,xf1).`, input: `writeq({- =xf1}).`, output: `syntax err.`},
		{name: "137", input: `writeq(- (a*b)).`, output: `- (a*b)`},
		{name: "138", input: `writeq(\ (a*b)).`, output: `\ (a*b)`},
		{name: "141", input: `current_op(P,xfy,.).`, output: `fails`},
		{name: "142", input: `op(100,xfy,.).`},
		{name: "143", premise: `op(100,xfy,.).`, input: `writeq(1 .2).`, output: `[1|2]`},
		{name: "144", premise: `op(100,xfy,.).`, input: `writeq([1]).`, output: `[1]`},
		{name: "283", premise: `op(100,xfy,.).`, input: `writeq(-[1]).`, output: `-[1]`},
		{name: "221", premise: `op(100,xfy,.).`, input: `X = 1.e, X = [1|e].`},
		{name: "258", premise: `op(100,xfy,.).`, input: `writeq(ok).%
1 = X.`, output: `ok`},
		{name: "145", input: `write_canonical('$VAR'(0)).`, output: `'$VAR'(0)`},
		{name: "146", input: `write_term('$VAR'(0),[]).`, output: `$VAR(0)`},
		{name: "244", input: `writeq('$VAR'(0)).`, output: `A`},
		{name: "245", input: `writeq('$VAR'(-1)).`, output: `'$VAR'(-1)`},
		{name: "246", input: `writeq('$VAR'(-2)).`, output: `'$VAR'(-2)`},
		{name: "247", input: `writeq('$VAR'(x)).`, output: `'$VAR'(x)`},
		{name: "289", input: `writeq('$VAR'('A')).`, output: `'$VAR'('A')`},
		{name: "147", premise: `op(9,fy,fy),op(9,yf,yf).`, input: `write_canonical(fy 1 yf).`, output: `fy(yf(1))`},
		{name: "148", premise: `op(9,fy,fy),op(9,yf,yf).`, input: `write_canonical(fy yf).`, output: `syntax err.`},
		{name: "149", premise: `op(9,fy,fy),op(9,yf,yf).`, input: `writeq(fy(yf(1))).`, output: `fy 1 yf`},
		{name: "150", premise: `op(9,fy,fy),op(9,yf,yf).`, input: `writeq(yf(fy(1))).`, output: `(fy 1)yf`},
		{name: "151", premise: `op(9,fy,fy),op(9,yfx,yfx).`, input: `write_canonical(fy 1 yfx 2).`, output: `fy(yfx(1,2))`},
		{name: "152", premise: `op(9,fy,fy),op(9,yfx,yfx).`, input: `writeq(fy(yfx(1,2))).`, output: `fy 1 yfx 2`},
		{name: "153", premise: `op(9,fy,fy),op(9,yfx,yfx).`, input: `writeq(yfx(fy(1),2)).`, output: `(fy 1)yfx 2`},
		{name: "154", premise: `op(9,yf,yf),op(9,xfy,xfy).`, input: `write_canonical(1 xfy 2 yf).`, output: `xfy(1,yf(2))`},
		{name: "155", premise: `op(9,yf,yf),op(9,xfy,xfy).`, input: `writeq(xfy(1,yf(2))).`, output: `1 xfy 2 yf`},
		{name: "156", premise: `op(9,yf,yf),op(9,xfy,xfy).`, input: `writeq(yf(xfy(1,2))).`, output: `(1 xfy 2)yf`},
		{name: "157", premise: `op(0,xfy,:-).`, input: `current_op(P,xfx,:-).`, output: `fails`},
		{name: "158", input: `op(0,xfy,',').`, output: `permission_error(modify,operator,',')`},
		{name: "159", premise: `op(9,fy,f),op(9,yf,f).`, input: `write_canonical(f f 0).`, output: `f(f(0))`},
		{name: "201", premise: `op(9,fy,f),op(9,yf,f).`, input: `writeq(f(f(0))).`, output: `f f 0`},
		{name: "202", premise: `op(9,fy,f),op(9,yf,f).`, input: `write_canonical(f 0 f).`, output: `f(f(0))`},
		{name: "160", premise: `op(9,fy,f),op(9,yf,f).`, input: `write_canonical(0 f f).`, output: `f(f(0))`},
		{name: "161", premise: `op(9,fy,f),op(9,yf,f).`, input: `write_canonical(f f).`, output: `syntax err.`},
		{name: "162", premise: `op(9,fy,p),op(9,yfx,p).`, input: `write_canonical(1 p p p 2).`, output: `syntax err.`},
		{name: "163", premise: `op(9,fy,p),op(9,xfy,p).`, input: `write_canonical(1 p p p 2).`, output: `p(1,p(p(2)))`},
		{name: "164", premise: `op(7,fy,p),op(9,yfx,p).`, input: `write_canonical(1 p p p 2).`, output: `p(1,p(p(2)))`},
		{name: "165", input: `atom('.''-''.').`},
		{name: "166", input: `op(0,xfy,'|').`},
		{name: "167", premise: `op(0,xfy,'|').`, input: `writeq((a|b)).`, output: `syntax err.`},
		{name: "168", input: `op(0,xfy,.),op(9,yf,.).`},
		{name: "169", premise: `op(0,xfy,.),op(9,yf,.).`, input: `writeq(.(.)).`, output: `('.')'.'`},
		{name: "194", input: `op(0,xfy,.),writeq((.)+(.)).`, output: `'.'+'.'`},
		{name: "170", input: `set_prolog_flag(double_quotes,chars).`},
		{name: "171", premise: `set_prolog_flag(double_quotes,chars).`, input: `writeq("a").`, output: `[a]`},
		{name: "229", premise: `set_prolog_flag(double_quotes,chars).`, input: `writeq("\z").`, output: `syntax err.`},
		{name: "300", premise: `set_prolog_flag(double_quotes,chars).`, input: `writeq("\0\").`, output: `['\x0\']`},
		{name: "172", input: `X is 10.0** -323, writeq(X).`, output: `1.0e-323`},
		{name: "173", input: `1.0e-323=:=10.0** -323.`},
		{name: "174", input: `-1 = -0x1.`},
		{name: "175", input: `T = t(0b1,0o1,0x1), T = t(1,1,1).`},
		{name: "176", input: `X is 0b1mod 2, X = 1.`},
		{name: "217", input: `op(1105,xfy,'|').`},
		{name: "181", premise: `op(1105,xfy,'|').`, input: `writeq((a-->b,c|d)).`, output: `a-->b,c|d`},
		{name: "290", premise: `op(1105,xfy,'|').`, input: `writeq([(a|b)]).`, output: `[(a|b)]`},
		{name: "186", input: `X/* /*/=7, X = 7.`},
		{name: "187", input: `X/*/*/=7, X = 7.`},
		{name: "198", input: `atom($-).`},
		{name: "199", input: `atom(-$).`},
		{name: "200", premise: `op(900, fy, [$]).`, input: `write_canonical($a+b).`, output: `$(+(a,b))`},
		{name: "224", input: `\ .`, output: `existence_error(procedure,(\)/0)`},
		{name: "225", input: `char_code(C,0), writeq(C).`, output: `'\x0\'`},
		{name: "250", input: `writeq('\0\').`, output: `'\x0\'`},
		{name: "226", input: `write_canonical(_+_).`, outputFn: func(t *testing.T, output string) {
			t.Helper()
			p, err := regexp.Compile(`\A\+\(_(\d+),_(\d+)\)\z`) // +(_1,_2)
			assert.NoError(t, err)
			match := p.FindAllStringSubmatch(output, 2)
			assert.NotEqual(t, match[0][1], match[0][2])
		}},
		{name: "227", input: `write_canonical(B+B).`, outputFn: func(t *testing.T, output string) {
			t.Helper()
			p, err := regexp.Compile(`\A\+\(_(\d+),_(\d+)\)\z`) // +(_1,_1)
			assert.NoError(t, err)
			match := p.FindAllStringSubmatch(output, 2)
			assert.Equal(t, match[0][1], match[0][2])
		}},
		{name: "228", input: `writeq(0'\z).`, output: `syntax err.`},
		{name: "230", input: `char_code('\^',X).`, output: `syntax err.`},
		{name: "231", input: `writeq(0'\c).`, output: `syntax err.`},
		{name: "232", input: `writeq(0'\ ).`, output: `syntax err.`},
		{name: "232", input: `writeq(nop (1)).`, output: `syntax err.`},
		{name: "234", premise: `op(400,fx,f).`, input: `writeq(f/*.*/(1,2)).`, output: `f (1,2)`},
		{name: "235", premise: `op(400,fx,f).`, input: `writeq(1 = f).`, output: `syntax err.`},
		{name: "236", input: `write_canonical(a- - -b).`, output: `-(a,-(-(b)))`},
		{name: "237", input: `op(699,xf,>).`, output: `permission_error(create,operator,>)`},
		{name: "238", premise: `\+catch(op(699,xf,>), error(permission_error(create,operator,>), _), false).`, input: `writeq(>(>(a),b)).`, output: `>(a)>b`},
		{name: "239", premise: `\+catch(op(699,xf,>), error(permission_error(create,operator,>), _), false).`, input: `write_canonical(a> >b).`, output: `syntax err.`},
		{name: "242", premise: `\+catch(op(699,xf,>), error(permission_error(create,operator,>), _), false).`, input: `write_canonical(a> =b).`, output: `syntax err.`},
		{name: "243", premise: `\+catch(op(699,xf,>), error(permission_error(create,operator,>), _), false).`, input: `write_canonical((a>,b)).`, output: `syntax err.`},
		{name: "240", premise: `\+catch(op(699,xf,>), error(permission_error(create,operator,>), _), false).`, input: `write_canonical(a>).`, output: `syntax err.`},
		{name: "251", premise: `op(9,yfx,[bop,bo,b,op,xor]).`, input: `writeq(0bop 2).`, output: `0 bop 2`},
		{name: "263", premise: `op(9,yfx,[bop,bo,b,op,xor]).`, input: `writeq(0 bop 2).`, output: `0 bop 2`},
		{name: "252", premise: `op(9,yfx,[bop,bo,b,op,xor]).`, input: `writeq(0 bop 2).`, output: `0 bop 2`},
		{name: "253", premise: `op(9,yfx,[bop,bo,b,op,xor]).`, input: `writeq(0b 2).`, output: `0 b 2`},
		{name: "254", premise: `op(9,yfx,[bop,bo,b,op,xor]).`, input: `writeq(0op 2).`, output: `0 op 2`},
		{name: "255", premise: `op(9,yfx,[bop,bo,b,op,xor]).`, input: `writeq(0xor 2).`, output: `0 xor 2`},
		{name: "264", input: "writeq('^`').", output: "'^`'"},
		{name: "265", input: `op(9,yf,[b2,o8]).`},
		{name: "266", premise: `op(9,yf,[b2,o8]).`, input: `writeq(0b2).`, output: `0 b2`},
		{name: "267", premise: `op(9,yf,[b2,o8]).`, input: `writeq(0o8).`, output: `0 o8`},
		{name: "268", input: `op(500, xfy, {}).`, output: `permission_error(create,operator,{})`},
		{name: "269", input: `writeq('\b\r\f\t\n').`, output: `'\b\r\f\t\n'`},
		{name: "270", input: `get_char(C), C = ' '. %a`},
		{name: "271", input: `get_char(C), C = '%'.%a`},
		{name: "272", input: `writeq(0B1).`, output: `syntax err.`},
		{name: "274", input: `op(20,fx,--),writeq(--(a)).`, output: `--a`},
		{name: "275", premise: `op(20,fx,--).`, input: `op(0,fy,--),writeq(--(a)).`, output: `--(a)`},
		{name: "276", input: `writeq(0xamod 2).`, output: `10 mod 2`},
		{name: "280", input: `writeq(00'+'1).`, output: `0+1`},
		{name: "281", input: `writeq(00'a).`, output: `syntax err.`},
		{name: "284", input: `writeq('\^J').`, output: `syntax err.`},
		{name: "291", input: `writeq([(a,b)]).`, output: `[(a,b)]`},
		{name: "292", input: `writeq(1= \\).`, output: `1= \\`},
		{name: "293", input: `writeq((,)).`, output: `syntax err.`},
		{name: "294", input: `writeq({[}).`, output: `syntax err.`},
		{name: "295", input: `writeq({(}).`, output: `syntax err.`},
		{name: "298", input: `writeq([a,b|c]).`, output: `[a,b|c]`},
		{name: "299", input: `(\+ (a,b)) = \+(T).`},
		{name: "302", input: `[] = '[]'.`},
		{name: "304", premise: `op(300,fy,~).`, input: `writeq(~ (a=b)).`, output: `~ (a=b)`},
		{name: "305", input: `writeq(\ (a=b)).`, output: `\ (a=b)`},
		{name: "306", input: `writeq(+ (a=b)).`, output: `+ (a=b)`},
		{name: "307", input: `writeq([/**/]).`, output: `[]`},
		{name: "308", input: `writeq(.+).`, output: `.+`},
		{name: "309", input: `writeq({a,b}).`, output: `{a,b}`},
		{name: "310", input: `writeq({\+ (}).`, output: `syntax err.`},
		{name: "311", input: `Finis ().`, output: `syntax err.`},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var out bytes.Buffer
			p := New(bytes.NewBufferString(tt.input+"\n"), &out)
			if tt.premise != "" {
				assert.NoError(t, p.QuerySolution(tt.premise).Err())
			}
			sol := p.QuerySolution(`
				catch(catch((read(X), X),
				            error(syntax_error(_), _),
				            write('syntax err.')),
				      error(E, _),
				      writeq(E)), !; write(fails).
			`)
			assert.NoError(t, sol.Err())
			if tt.outputFn != nil {
				tt.outputFn(t, out.String())
			} else {
				assert.Equal(t, tt.output, out.String())
			}
		})
	}
}

func TestInterpreter_Exec(t *testing.T) {
	tests := []struct {
		query   string
		args    []interface{}
		err     bool
		premise string
	}{
		{query: `append(nil, L, L).`},
		{query: `0.`, err: true},
		{query: `append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).`},

		{query: `foo(?, ?, ?, ?).`, args: []interface{}{"a", 1, 2.0, []string{"abc", "def"}}},
		{query: `foo(?).`, args: []interface{}{nil}, err: true},

		{query: `#!/usr/bin/env 1pl
append(nil, L, L).`},
		{query: `#!/usr/bin/env 1pl`},

		{query: `a.`, premise: `term_expansion(_, _) :- throw(fail).`, err: true},

		{query: `:- true.`},
		{query: `:- fail.`, err: true},
		{query: `:- throw(ball).`, err: true},

		{query: `:- initialization(true).`},
		{query: `:- initialization(fail).`, err: true},
		{query: `:- initialization(throw(ball)).`, err: true},
	}

	for _, tt := range tests {
		t.Run(tt.query, func(t *testing.T) {
			var i Interpreter
			i.Register0(engine.NewAtom("true"), func(_ *engine.VM, k engine.Cont, env *engine.Env) *engine.Promise {
				return k(env)
			})
			i.Register0(engine.NewAtom("fail"), func(*engine.VM, engine.Cont, *engine.Env) *engine.Promise {
				return engine.Bool(false)
			})
			i.Register1(engine.NewAtom("consult"), engine.Consult)
			i.Register3(engine.NewAtom("op"), engine.Op)
			assert.NoError(t, i.Exec(`:-(op(1200, xfx, :-)).`))
			assert.NoError(t, i.Exec(`:-(op(1200, fx, :-)).`))
			assert.NoError(t, i.Exec(tt.premise))
			if tt.err {
				assert.Error(t, i.Exec(tt.query, tt.args...))
			} else {
				assert.NoError(t, i.Exec(tt.query, tt.args...))
			}
		})
	}
}

func TestInterpreter_Query(t *testing.T) {
	type result struct {
		A    string
		B    int
		C    float64
		List []string `prolog:"D"`
	}

	tests := []struct {
		query             string
		args              []interface{}
		queryErr, scanErr bool
		scan              interface{}
		result            func() interface{}
	}{
		{query: `append(X, Y, Z).`, scan: map[string]engine.Term{}, result: func() interface{} {
			last := engine.NewVariable()
			return map[string]engine.Term{
				"X": engine.NewAtom("nil"),
				"Y": last - 16,
				"Z": last - 16,
			}
		}},
		{query: `append(cons(a, cons(b, nil)), cons(c, nil), X).`, scan: map[string]engine.Term{}, result: func() interface{} {
			return map[string]engine.Term{
				"X": engine.NewAtom("cons").Apply(
					engine.NewAtom("a"),
					engine.NewAtom("cons").Apply(
						engine.NewAtom("b"),
						engine.NewAtom("cons").Apply(
							engine.NewAtom("c"),
							engine.NewAtom("nil"),
						),
					),
				),
			}
		}},
		{query: `foo(?, ?, ?, ?).`, args: []interface{}{"a", 1, 2.0, []string{"abc", "def"}}, scan: map[string]interface{}{}, result: func() interface{} {
			return map[string]interface{}{}
		}},
		{query: `foo(?, ?, ?, ?).`, args: []interface{}{nil, 1, 2.0, []string{"abc", "def"}}, queryErr: true, result: func() interface{} { return nil }},
		{query: `foo(A, B, C, D).`, scan: &result{}, result: func() interface{} {
			return &result{
				A:    "a",
				B:    1,
				C:    2.0,
				List: []string{"abc", "def"},
			}
		}},
	}

	for _, tt := range tests {
		t.Run(tt.query, func(t *testing.T) {
			var i Interpreter
			i.Register3(engine.NewAtom("op"), engine.Op)
			assert.NoError(t, i.Exec(`
:-(op(1200, xfx, :-)).

append(nil, L, L).
append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).

foo(a, 1, 2.0, [abc, def]).
`))

			sols, err := i.Query(tt.query, tt.args...)
			if tt.queryErr {
				assert.Error(t, err)
				return
			} else {
				assert.NoError(t, err)
			}

			assert.True(t, sols.Next())
			if tt.scanErr {
				assert.Error(t, sols.Scan(tt.scan))
				return
			} else {
				assert.NoError(t, sols.Scan(tt.scan))
			}
			assert.Equal(t, tt.result(), tt.scan)
		})
	}
}

func TestInterpreter_Query_close(t *testing.T) {
	var i Interpreter
	i.Register0(engine.NewAtom("do_not_call"), func(_ *engine.VM, k engine.Cont, env *engine.Env) *engine.Promise {
		assert.Fail(t, "unreachable")
		return k(env)
	})
	sols, err := i.Query("do_not_call.")
	assert.NoError(t, err)
	assert.NoError(t, sols.Close())
}

func TestMisc(t *testing.T) {
	t.Run("negation", func(t *testing.T) {
		i := New(nil, nil)
		sols, err := i.Query(`\+true.`)
		assert.NoError(t, err)

		assert.False(t, sols.Next())
	})

	t.Run("cut", func(t *testing.T) {
		// https://www.cs.uleth.ca/~gaur/post/prolog-cut-negation/
		t.Run("p", func(t *testing.T) {
			i := New(nil, nil)
			assert.NoError(t, i.Exec(`
p(a).
p(b):-!.
p(c).
`))

			t.Run("single", func(t *testing.T) {
				sols, err := i.Query(`p(X).`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				var s struct {
					X string
				}

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "a", s.X)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "b", s.X)

				assert.False(t, sols.Next())
			})

			t.Run("double", func(t *testing.T) {
				sols, err := i.Query(`p(X), p(Y).`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				var s struct {
					X string
					Y string
				}

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "a", s.X)
				assert.Equal(t, "a", s.Y)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "a", s.X)
				assert.Equal(t, "b", s.Y)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "b", s.X)
				assert.Equal(t, "a", s.Y)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "b", s.X)
				assert.Equal(t, "b", s.Y)

				assert.False(t, sols.Next())
			})
		})

		// http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html
		t.Run("teaches", func(t *testing.T) {
			i := New(nil, nil)
			assert.NoError(t, i.Exec(`
teaches(dr_fred, history).
teaches(dr_fred, english).
teaches(dr_fred, drama).
teaches(dr_fiona, physics).
studies(alice, english).
studies(angus, english).
studies(amelia, drama).
studies(alex, physics).
`))

			t.Run("without cut", func(t *testing.T) {
				sols, err := i.Query(`teaches(dr_fred, Course), studies(Student, Course).`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				type cs struct {
					Course  string
					Student string
				}
				var s cs

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, cs{
					Course:  "english",
					Student: "alice",
				}, s)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, cs{
					Course:  "english",
					Student: "angus",
				}, s)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, cs{
					Course:  "drama",
					Student: "amelia",
				}, s)

				assert.False(t, sols.Next())
			})

			t.Run("with cut in the middle", func(t *testing.T) {
				sols, err := i.Query(`teaches(dr_fred, Course), !, studies(Student, Course).`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				assert.False(t, sols.Next())
			})

			t.Run("with cut at the end", func(t *testing.T) {
				sols, err := i.Query(`teaches(dr_fred, Course), studies(Student, Course), !.`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				type cs struct {
					Course  string
					Student string
				}
				var s cs

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, cs{
					Course:  "english",
					Student: "alice",
				}, s)

				assert.False(t, sols.Next())
			})

			t.Run("with cut at the beginning", func(t *testing.T) {
				sols, err := i.Query(`!, teaches(dr_fred, Course), studies(Student, Course).`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				type cs struct {
					Course  string
					Student string
				}
				var s cs

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, cs{
					Course:  "english",
					Student: "alice",
				}, s)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, cs{
					Course:  "english",
					Student: "angus",
				}, s)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, cs{
					Course:  "drama",
					Student: "amelia",
				}, s)

				assert.False(t, sols.Next())
			})
		})

		t.Run("call/1 makes a difference", func(t *testing.T) {
			t.Run("with", func(t *testing.T) {
				i := New(nil, nil)
				sols, err := i.Query(`call(!), fail; true.`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				assert.True(t, sols.Next())
			})

			t.Run("without", func(t *testing.T) {
				i := New(nil, nil)
				sols, err := i.Query(`!, fail; true.`)
				assert.NoError(t, err)
				defer func() {
					assert.NoError(t, sols.Close())
				}()

				assert.False(t, sols.Next())
			})
		})
	})

	t.Run("repeat", func(t *testing.T) {
		t.Run("cut", func(t *testing.T) {
			i := New(nil, nil)
			sols, err := i.Query("repeat, !, fail.")
			assert.NoError(t, err)
			assert.False(t, sols.Next())
		})

		t.Run("stream", func(t *testing.T) {
			i := New(nil, nil)
			sols, err := i.Query("repeat, (X = a; X = b).")
			assert.NoError(t, err)

			var s struct {
				X string
			}

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "a", s.X)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "b", s.X)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "a", s.X)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "b", s.X)
		})
	})

	t.Run("atom_chars", func(t *testing.T) {
		i := New(nil, nil)
		sols, err := i.Query("atom_chars(f(a), L).")
		assert.NoError(t, err)
		assert.False(t, sols.Next())
	})

	t.Run("term_eq", func(t *testing.T) {
		i := New(nil, nil)
		sols, err := i.Query("f(a) == f(a).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
	})

	t.Run("call cut", func(t *testing.T) {
		i := New(nil, nil)
		assert.NoError(t, i.Exec(`
foo :- call(true), !.
foo :- throw(unreachable).
`))
		sols, err := i.Query("foo.")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
	})

	t.Run("catch cut", func(t *testing.T) {
		i := New(nil, nil)
		assert.NoError(t, i.Exec(`
foo :- catch(true, _, true), !.
foo :- throw(unreachable).
`))
		sols, err := i.Query("foo.")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
	})

	t.Run("counter", func(t *testing.T) {
		i := New(nil, nil)
		assert.NoError(t, i.Exec(`
:- dynamic(count/1).
count(0).

next(N) :- retract(count(X)), N is X + 1, asserta(count(N)).
`))

		var s struct {
			X int
		}

		sols, err := i.Query("next(X).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 1, s.X)
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
		assert.NoError(t, sols.Close())

		sols, err = i.Query("next(X).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 2, s.X)
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
		assert.NoError(t, sols.Close())

		sols, err = i.Query("next(X).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 3, s.X)
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
		assert.NoError(t, sols.Close())
	})
}

func TestInterpreter_QuerySolution(t *testing.T) {
	var i Interpreter
	assert.NoError(t, i.Exec(`
foo(a, b).
foo(b, c).
foo(c, d).
`))

	t.Run("ok", func(t *testing.T) {
		t.Run("struct", func(t *testing.T) {
			sol := i.QuerySolution(`foo(X, Y).`)

			var s struct {
				X   string
				Foo string `prolog:"Y"`
			}
			assert.NoError(t, sol.Scan(&s))
			assert.Equal(t, "a", s.X)
			assert.Equal(t, "b", s.Foo)
		})

		t.Run("map", func(t *testing.T) {
			sol := i.QuerySolution(`foo(X, Y).`)

			m := map[string]string{}
			assert.NoError(t, sol.Scan(m))
			assert.Equal(t, []string{"X", "Y"}, sol.Vars())
			assert.Equal(t, "a", m["X"])
			assert.Equal(t, "b", m["Y"])
		})
	})

	t.Run("invalid query", func(t *testing.T) {
		sol := i.QuerySolution(``)
		assert.Error(t, sol.Err())
	})

	t.Run("no solutions", func(t *testing.T) {
		sol := i.QuerySolution(`foo(e, f).`)
		assert.Equal(t, ErrNoSolutions, sol.Err())
		assert.Empty(t, sol.Vars())
	})

	t.Run("runtime error", func(t *testing.T) {
		err := errors.New("something went wrong")

		i.Register0(engine.NewAtom("error"), func(_ *engine.VM, k engine.Cont, env *engine.Env) *engine.Promise {
			return engine.Error(err)
		})
		sol := i.QuerySolution(`error.`)
		assert.Equal(t, err, sol.Err())

		var s struct{}
		assert.Error(t, sol.Scan(&s))
	})
}

func ExampleInterpreter_Exec_placeholders() {
	p := New(nil, os.Stdout)

	_ = p.Exec(`my_atom(?).`, "foo")
	sols, _ := p.Query(`my_atom(A), atom(A), write(A), nl.`)
	sols.Next()
	_ = sols.Close()

	_ = p.Exec(`my_int(?, ?, ?, ?, ?).`, int8(1), int16(1), int32(1), int64(1), 1)
	sols, _ = p.Query(`my_int(I, I, I, I, I), integer(I), write(I), nl.`)
	sols.Next()
	_ = sols.Close()

	_ = p.Exec(`my_float(?, ?).`, float32(1), float64(1))
	sols, _ = p.Query(`my_float(F, F), float(F), write(F), nl.`)
	sols.Next()
	_ = sols.Close()

	_ = p.Exec(`my_atom_list(?).`, []string{"foo", "bar", "baz"})
	sols, _ = p.Query(`my_atom_list(As), maplist(atom, As), write(As), nl.`)
	sols.Next()
	_ = sols.Close()

	_ = p.Exec(`my_int_list(?).`, []int{1, 2, 3})
	sols, _ = p.Query(`my_int_list(Is), maplist(integer, Is), write(Is), nl.`)
	sols.Next()
	_ = sols.Close()

	_ = p.Exec(`my_float_list(?).`, []float64{1, 2, 3})
	sols, _ = p.Query(`my_float_list(Fs), maplist(float, Fs), write(Fs), nl.`)
	sols.Next()
	_ = sols.Close()

	// Output:
	// foo
	// 1
	// 1.0
	// [foo,bar,baz]
	// [1,2,3]
	// [1.0,2.0,3.0]
}

func ExampleInterpreter_Query_placeholders() {
	p := New(nil, os.Stdout)
	sols, _ := p.Query(`A = ?, atom(A), write(A), nl.`, "foo")
	sols.Next()
	_ = sols.Close()
	sols, _ = p.Query(`(I, I, I, I, I) = (?, ?, ?, ?, ?), integer(I), write(I), nl.`, int8(1), int16(1), int32(1), int64(1), 1)
	sols.Next()
	_ = sols.Close()
	sols, _ = p.Query(`(F, F) = (?, ?), float(F), write(F), nl.`, float32(1), float64(1))
	sols.Next()
	_ = sols.Close()
	sols, _ = p.Query(`L = ?, maplist(atom, L), write(L), nl.`, []string{"foo", "bar", "baz"})
	sols.Next()
	_ = sols.Close()
	sols, _ = p.Query(`L = ?, maplist(integer, L), write(L), nl.`, []int{1, 2, 3})
	sols.Next()
	_ = sols.Close()
	sols, _ = p.Query(`L = ?, maplist(float, L), write(L), nl.`, []float64{1, 2, 3})
	sols.Next()
	_ = sols.Close()

	// Output:
	// foo
	// 1
	// 1.0
	// [foo,bar,baz]
	// [1,2,3]
	// [1.0,2.0,3.0]
}

func ExampleNew_phrase() {
	p := New(nil, nil)
	_ = p.Exec(`
determiner --> [the].
determiner --> [a].

noun --> [boy].
noun --> [girl].

verb --> [likes].
verb --> [scares].

noun_phrase --> determiner, noun.
noun_phrase --> noun.

verb_phrase --> verb.
verb_phrase --> verb, noun_phrase.

sentence --> noun_phrase, verb_phrase.
`)

	sols, _ := p.Query(`phrase([the], [the]).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`phrase(sentence, [the, girl, likes, the, boy]).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`phrase(sentence, [the, girl, likes, the, boy, today]).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`phrase(sentence, [the, girl, likes]).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`phrase(sentence, Sentence).`)
	for sols.Next() {
		var s struct {
			Sentence []string
		}
		_ = sols.Scan(&s)
		fmt.Printf("Sentence = %s\n", s.Sentence)
		break // Many other sentences follow.
	}
	_ = sols.Close()

	sols, _ = p.Query(`phrase(noun_phrase, [the, girl, scares, the, boy], Rest).`)
	for sols.Next() {
		var s struct {
			Rest []string
		}
		_ = sols.Scan(&s)
		fmt.Printf("Rest = %s\n", s.Rest)
	}
	_ = sols.Close()

	// Output:
	// true
	// true
	// false
	// true
	// Sentence = [the boy likes]
	// Rest = [scares the boy]
}

func ExampleNew_subsumes_term() {
	p := New(nil, nil)

	sols, _ := p.Query(`subsumes_term(a, a).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`subsumes_term(f(X,Y), f(Z,Z)).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`subsumes_term(f(Z,Z), f(X,Y)).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`subsumes_term(g(X), g(f(X))).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`subsumes_term(X, f(X)).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`subsumes_term(X, Y), subsumes_term(Y, f(X)).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	// Output:
	// true
	// true
	// false
	// false
	// false
	// true
}

func ExampleNew_callable() {
	p := New(nil, nil)

	sols, _ := p.Query(`callable(a).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`callable(3).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`callable(X).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`callable((1,2)).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	// Output:
	// true
	// false
	// false
	// true
}

func ExampleNew_acyclicTerm() {
	p := New(nil, nil)

	sols, _ := p.Query(`acyclic_term(a(1, _)).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`X = f(X), acyclic_term(X).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	// Output:
	// true
	// false
}

func ExampleNew_ground() {
	p := New(nil, nil)

	sols, _ := p.Query(`ground(3).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`ground(a(1, _)).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	// Output:
	// true
	// false
}

func ExampleNew_sort() {
	p := New(nil, nil)

	sols, _ := p.Query(`sort([1, 1], Sorted).`)
	for sols.Next() {
		var s struct {
			Sorted []int
		}
		_ = sols.Scan(&s)
		fmt.Printf("Sorted = %d\n", s.Sorted)
	}
	_ = sols.Close()

	sols, _ = p.Query(`sort([X, 1], [1, 1]).`)
	for sols.Next() {
		var s struct {
			X int
		}
		_ = sols.Scan(&s)
		fmt.Printf("X = %d\n", s.X)
	}
	_ = sols.Close()

	sols, _ = p.Query(`sort([1, 1], [1, 1]).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`sort([V], V).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`sort([f(U),U,U,f(V),f(U),V],L).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	// Output:
	// Sorted = [1]
	// X = 1
	// false
	// true
	// true
}

func ExampleNew_arg() {
	p := New(nil, nil)

	sols, _ := p.Query(`arg(1, foo(a, b), a).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`arg(1, foo(a, b), X).`)
	for sols.Next() {
		var s struct {
			X string
		}
		_ = sols.Scan(&s)
		fmt.Printf("X = %s\n", s.X)
	}
	_ = sols.Close()

	sols, _ = p.Query(`arg(1, foo(X, b), a).`)
	for sols.Next() {
		var s struct {
			X string
		}
		_ = sols.Scan(&s)
		fmt.Printf("X = %s\n", s.X)
	}
	_ = sols.Close()

	sols, _ = p.Query(`arg(1, foo(X, b), Y), X = a.`)
	for sols.Next() {
		var s struct {
			X, Y string
		}
		_ = sols.Scan(&s)
		fmt.Printf("X = %s, Y = %s\n", s.X, s.Y)
	}
	_ = sols.Close()

	sols, _ = p.Query(`arg(1, foo(a, b), b).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`arg(0, foo(a, b), foo).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`arg(3, foo(a, b), N).`)
	fmt.Printf("%t\n", sols.Next())
	_ = sols.Close()

	sols, _ = p.Query(`arg(X, foo(a, b), a).`)
	sols.Next()
	fmt.Printf("%v\n", sols.Err())
	_ = sols.Close()

	sols, _ = p.Query(`arg(1, X, a).`)
	sols.Next()
	fmt.Printf("%v\n", sols.Err())
	_ = sols.Close()

	sols, _ = p.Query(`arg(0, atom, A).`)
	sols.Next()
	fmt.Printf("%v\n", sols.Err())
	_ = sols.Close()

	sols, _ = p.Query(`arg(0, 3, A).`)
	sols.Next()
	fmt.Printf("%v\n", sols.Err())
	_ = sols.Close()

	// Output:
	// true
	// X = a
	// X = a
	// X = a, Y = a
	// false
	// false
	// false
	// error(instantiation_error,arg/3)
	// error(instantiation_error,arg/3)
	// error(type_error(compound,atom),arg/3)
	// error(type_error(compound,3),arg/3)
}

func TestDefaultFS_Open(t *testing.T) {
	var fs defaultFS
	f, err := fs.Open("interpreter.go")
	assert.NoError(t, err)
	assert.NotNil(t, f)
}

type readFn func(p []byte) (n int, err error)

func (f readFn) Read(p []byte) (n int, err error) {
	return f(p)
}
