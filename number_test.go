package prolog

import (
	"context"
	"github.com/ichiban/prolog/internal"
	"io"
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestIs(t *testing.T) {
	foo := internal.NewAtom("foo")

	tests := []struct {
		title              string
		result, expression Term
		ok                 bool
		err                error
	}{
		{title: "integer", result: Integer(1), expression: Integer(1), ok: true},
		{title: "float", result: Float(1), expression: Float(1), ok: true},

		{title: "pi", result: Float(math.Pi), expression: atomPi, ok: true},

		{title: "1 + 1", result: Integer(2), expression: atomPlus.Apply(Integer(1), Integer(1)), ok: true},
		{title: "maxInt + 1", expression: atomPlus.Apply(Integer(math.MaxInt64), Integer(1)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "minInt - 1", expression: atomPlus.Apply(Integer(math.MinInt64), Integer(-1)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "1 + 1.0", result: Float(2), expression: atomPlus.Apply(Integer(1), Float(1)), ok: true},
		{title: "1.0 + 1", result: Float(2), expression: atomPlus.Apply(Float(1), Integer(1)), ok: true},
		{title: "1.0 + maxFloat", expression: atomPlus.Apply(Float(1), Float(math.MaxFloat64)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "-1.0 + -maxFloat", expression: atomPlus.Apply(Float(-1), Float(-math.MaxFloat64)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "mock + mock", expression: atomPlus.Apply(&mockNumber{}, &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "1 - 1", result: Integer(0), expression: atomMinus.Apply(Integer(1), Integer(1)), ok: true},
		{title: "maxInt - -1", expression: atomMinus.Apply(Integer(math.MaxInt64), Integer(-1)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "minInt - 1", expression: atomMinus.Apply(Integer(math.MinInt64), Integer(1)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "1 - 1.0", result: Float(0), expression: atomMinus.Apply(Integer(1), Float(1)), ok: true},
		{title: "1.0 - 1", result: Float(0), expression: atomMinus.Apply(Float(1), Integer(1)), ok: true},
		{title: "1.0 - 1.0", result: Float(0), expression: atomMinus.Apply(Float(1), Float(1)), ok: true},
		{title: "mock - mock", expression: atomMinus.Apply(&mockNumber{}, &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "1 * 1", result: Integer(1), expression: atomAsterisk.Apply(Integer(1), Integer(1)), ok: true},
		{title: "maxInt * 2", expression: atomAsterisk.Apply(Integer(math.MaxInt64), Integer(2)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "1 * 0", result: Integer(0), expression: atomAsterisk.Apply(Integer(1), Integer(0)), ok: true},
		{title: "-1 * minInt", expression: atomAsterisk.Apply(Integer(-1), Integer(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "minInt * -1", expression: atomAsterisk.Apply(Integer(math.MinInt64), Integer(-1)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "1 * 1.0", result: Float(1), expression: atomAsterisk.Apply(Integer(1), Float(1)), ok: true},
		{title: "1.0 * 1", result: Float(1), expression: atomAsterisk.Apply(Float(1), Integer(1)), ok: true},
		{title: "0.5 * ε", expression: atomAsterisk.Apply(Float(0.5), Float(math.SmallestNonzeroFloat64)), err: evaluationError(exceptionalValueUnderflow, nil)},
		{title: "maxFloat * 2", expression: atomAsterisk.Apply(Float(math.MaxFloat64), Integer(2)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "-maxFloat * 2", expression: atomAsterisk.Apply(Float(-math.MaxFloat64), Integer(2)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "mock * mock", expression: atomAsterisk.Apply(&mockNumber{}, &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "1 // 1", result: Integer(1), expression: atomSlashSlash.Apply(Integer(1), Integer(1)), ok: true},
		{title: "1 // 0", expression: atomSlashSlash.Apply(Integer(1), Integer(0)), err: evaluationError(exceptionalValueZeroDivisor, nil)},
		{title: "minInt // -1", expression: atomSlashSlash.Apply(Integer(math.MinInt64), Integer(-1)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "1.0 // 1", expression: atomSlashSlash.Apply(Float(1), Integer(1)), err: typeError(validTypeInteger, Float(1), nil)},
		{title: "1 // 1.0", expression: atomSlashSlash.Apply(Integer(1), Float(1)), err: typeError(validTypeInteger, Float(1), nil)},

		{title: "1 / 1", result: Float(1), expression: atomSlash.Apply(Integer(1), Integer(1)), ok: true},
		{title: "1.0 / 1", result: Float(1), expression: atomSlash.Apply(Float(1), Integer(1)), ok: true},
		{title: "1 / 1.0", result: Float(1), expression: atomSlash.Apply(Integer(1), Float(1)), ok: true},
		{title: "1 / 0", expression: atomSlash.Apply(Integer(1), Integer(0)), err: evaluationError(exceptionalValueZeroDivisor, nil)},
		{title: "maxFloat / 0.5", expression: atomSlash.Apply(Float(math.MaxFloat64), Float(0.5)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "-maxFloat / 0.5", expression: atomSlash.Apply(Float(-math.MaxFloat64), Float(0.5)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "ε / 2.0", expression: atomSlash.Apply(Float(math.SmallestNonzeroFloat64), Float(2)), err: evaluationError(exceptionalValueUnderflow, nil)},
		{title: "1 div mock", expression: atomSlash.Apply(Integer(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "mock div 1", expression: atomSlash.Apply(&mockNumber{}, Integer(1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "1 rem 1", result: Integer(0), expression: atomRem.Apply(Integer(1), Integer(1)), ok: true},
		{title: "1 rem 0", expression: atomRem.Apply(Integer(1), Integer(0)), err: evaluationError(exceptionalValueZeroDivisor, nil)},
		{title: "1.0 rem 1", expression: atomRem.Apply(Float(1), Integer(1)), err: typeError(validTypeInteger, Float(1), nil)},
		{title: "1 rem 1.0", expression: atomRem.Apply(Integer(1), Float(1)), err: typeError(validTypeInteger, Float(1), nil)},

		{title: "1 mod 1", result: Integer(0), expression: atomMod.Apply(Integer(1), Integer(1)), ok: true},
		{title: "1 mod 0", expression: atomMod.Apply(Integer(1), Integer(0)), err: evaluationError(exceptionalValueZeroDivisor, nil)},
		{title: "1.0 mod 1", expression: atomMod.Apply(Float(1), Integer(1)), err: typeError(validTypeInteger, Float(1), nil)},
		{title: "1 mod 1.0", expression: atomMod.Apply(Integer(1), Float(1)), err: typeError(validTypeInteger, Float(1), nil)},

		{title: "- 1", result: Integer(-1), expression: atomMinus.Apply(Integer(1)), ok: true},
		{title: "- 1.0", result: Float(-1), expression: atomMinus.Apply(Float(1)), ok: true},
		{title: "- minInt", expression: atomMinus.Apply(Integer(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "- mock", expression: atomMinus.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "abs(1)", result: Integer(1), expression: atomAbs.Apply(Integer(1)), ok: true},
		{title: "abs(-1)", result: Integer(1), expression: atomAbs.Apply(Integer(-1)), ok: true},
		{title: "abs(-1.0)", result: Float(1), expression: atomAbs.Apply(Float(-1)), ok: true},
		{title: "abs(minInt)", expression: atomAbs.Apply(Integer(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "abs(mock)", expression: atomAbs.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "sign(5)", result: Integer(1), expression: atomSign.Apply(Integer(5)), ok: true},
		{title: "sign(0)", result: Integer(0), expression: atomSign.Apply(Integer(0)), ok: true},
		{title: "sign(-5)", result: Integer(-1), expression: atomSign.Apply(Integer(-5)), ok: true},
		{title: "sign(5.0)", result: Float(1), expression: atomSign.Apply(Float(5)), ok: true},
		{title: "sign(0.0)", result: Float(0), expression: atomSign.Apply(Float(0)), ok: true},
		{title: "sign(-5.0)", result: Float(-1), expression: atomSign.Apply(Float(-5)), ok: true},
		{title: "sign(mock)", expression: atomSign.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "float_integer_part(1.23)", result: Float(1), expression: atomFloatIntegerPart.Apply(Float(1.23)), ok: true},
		{title: "float_integer_part(1)", expression: atomFloatIntegerPart.Apply(Integer(1)), err: typeError(validTypeFloat, Integer(1), nil)},

		{title: "float_fractional_part(1.23)", result: Float(0.22999999999999998), expression: atomFloatFractionalPart.Apply(Float(1.23)), ok: true},
		{title: "float_fractional_part(1)", expression: atomFloatFractionalPart.Apply(Integer(1)), err: typeError(validTypeFloat, Integer(1), nil)},

		{title: "float(1)", result: Float(1), expression: atomFloat.Apply(Integer(1)), ok: true},
		{title: "float(1.0)", result: Float(1), expression: atomFloat.Apply(Float(1)), ok: true},
		{title: "float(mock)", expression: atomFloat.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "floor(1.9)", result: Integer(1), expression: atomFloor.Apply(Float(1.9)), ok: true},
		{title: "floor(2.0 * maxInt)", expression: atomFloor.Apply(2 * Float(math.MaxInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "floor(2.0 * minInt)", expression: atomFloor.Apply(2 * Float(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "floor(1)", expression: atomFloor.Apply(Integer(1)), err: typeError(validTypeFloat, Integer(1), nil)},

		{title: "truncate(1.9)", result: Integer(1), expression: atomTruncate.Apply(Float(1.9)), ok: true},
		{title: "truncate(2.0 * maxInt)", expression: atomTruncate.Apply(2 * Float(math.MaxInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "truncate(2.0 * minInt)", expression: atomTruncate.Apply(2 * Float(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "truncate(1)", expression: atomTruncate.Apply(Integer(1)), err: typeError(validTypeFloat, Integer(1), nil)},

		{title: "round(1.9)", result: Integer(2), expression: atomRound.Apply(Float(1.9)), ok: true},
		{title: "round(2.0 * maxInt)", expression: atomRound.Apply(2 * Float(math.MaxInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "round(2.0 * minInt)", expression: atomRound.Apply(2 * Float(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "round(1)", expression: atomRound.Apply(Integer(1)), err: typeError(validTypeFloat, Integer(1), nil)},

		{title: "ceiling(1.9)", result: Integer(2), expression: atomCeiling.Apply(Float(1.9)), ok: true},
		{title: "ceiling(2.0 * maxInt)", expression: atomCeiling.Apply(2 * Float(math.MaxInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "ceiling(2.0 * minInt)", expression: atomCeiling.Apply(2 * Float(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "ceiling(1)", expression: atomCeiling.Apply(Integer(1)), err: typeError(validTypeFloat, Integer(1), nil)},

		{title: "1 div 1", result: Integer(1), expression: atomDiv.Apply(Integer(1), Integer(1)), ok: true},
		{title: "1 div 0", expression: atomDiv.Apply(Integer(1), Integer(0)), err: evaluationError(exceptionalValueZeroDivisor, nil)},
		{title: "minInt div -1", expression: atomDiv.Apply(Integer(math.MinInt64), Integer(-1)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "1.0 div 1", expression: atomDiv.Apply(Float(1), Integer(1)), err: typeError(validTypeInteger, Float(1), nil)},
		{title: "1 div 1.0", expression: atomDiv.Apply(Integer(1), Float(1)), err: typeError(validTypeInteger, Float(1), nil)},

		{title: "+ 1", result: Integer(1), expression: atomPlus.Apply(Integer(1)), ok: true},
		{title: "+ 1.0", result: Float(1), expression: atomPlus.Apply(Float(1)), ok: true},
		{title: "+ mock", expression: atomPlus.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "invalid unary argument", expression: atomMinus.Apply(&internal.mockTerm{}), err: typeError(validTypeEvaluable, atomSlash.Apply(&internal.mockTerm{}, Integer(0)), nil)},
		{title: "invalid binary argument: x", expression: atomMinus.Apply(&internal.mockTerm{}, Integer(0)), err: typeError(validTypeEvaluable, atomSlash.Apply(&internal.mockTerm{}, Integer(0)), nil)},
		{title: "invalid binary argument: y", expression: atomMinus.Apply(Integer(0), &internal.mockTerm{}), err: typeError(validTypeEvaluable, atomSlash.Apply(&internal.mockTerm{}, Integer(0)), nil)},
		{title: "unknown constant", expression: foo, err: typeError(validTypeEvaluable, atomSlash.Apply(foo, Integer(0)), nil)},
		{title: "unknown unary", expression: foo.Apply(Integer(1)), err: typeError(validTypeEvaluable, atomSlash.Apply(foo, Integer(1)), nil)},
		{title: "unknown binary", expression: foo.Apply(Integer(1), Integer(2)), err: typeError(validTypeEvaluable, atomSlash.Apply(foo, Integer(2)), nil)},
		{title: "Arity is more than 2", expression: foo.Apply(Integer(1), Integer(2), Integer(3)), err: typeError(validTypeEvaluable, atomSlash.Apply(foo, Integer(3)), nil)},

		// 8.6.1.3 Errors
		{title: "a", result: internal.NewVariable(), expression: internal.NewVariable(), err: InstantiationError(nil)},

		{title: "1 ** 1", result: Float(1), expression: atomAsteriskAsterisk.Apply(Integer(1), Integer(1)), ok: true},
		{title: "1 ** 1.0", result: Float(1), expression: atomAsteriskAsterisk.Apply(Integer(1), Float(1)), ok: true},
		{title: "1.0 ** 1", result: Float(1), expression: atomAsteriskAsterisk.Apply(Float(1), Integer(1)), ok: true},
		{title: "1 ** mock", expression: atomAsteriskAsterisk.Apply(Integer(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "mock ** 1", expression: atomAsteriskAsterisk.Apply(&mockNumber{}, Integer(1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "maxFloat ** 2.0", expression: atomAsteriskAsterisk.Apply(Float(math.MaxFloat64), Float(2)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "ε ** 2.0", expression: atomAsteriskAsterisk.Apply(Float(math.SmallestNonzeroFloat64), Float(2)), err: evaluationError(exceptionalValueUnderflow, nil)},
		{title: "-1 ** 1.1", expression: atomAsteriskAsterisk.Apply(Integer(-1), Float(1.1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "0 ** -1", expression: atomAsteriskAsterisk.Apply(Integer(0), Float(-1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "sin(0)", result: Float(0), expression: atomSin.Apply(Integer(0)), ok: true},
		{title: "sin(0.0)", result: Float(0), expression: atomSin.Apply(Float(0)), ok: true},
		{title: "sin(mock)", expression: atomSin.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "cos(0)", result: Float(1), expression: atomCos.Apply(Integer(0)), ok: true},
		{title: "cos(0.0)", result: Float(1), expression: atomCos.Apply(Float(0)), ok: true},
		{title: "cos(mock)", expression: atomCos.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "atan(0)", result: Float(0), expression: atomAtan.Apply(Integer(0)), ok: true},
		{title: "atan(0.0)", result: Float(0), expression: atomAtan.Apply(Float(0)), ok: true},
		{title: "atan(mock)", expression: atomAtan.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "exp(0)", result: Float(1), expression: atomExp.Apply(Integer(0)), ok: true},
		{title: "exp(0.0)", result: Float(1), expression: atomExp.Apply(Float(0)), ok: true},
		{title: "exp(mock)", expression: atomExp.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "exp(maxFloat)", expression: atomExp.Apply(Float(math.MaxFloat64)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "exp(-maxFloat)", expression: atomExp.Apply(Float(-math.MaxFloat64)), err: evaluationError(exceptionalValueUnderflow, nil)},

		{title: "log(1)", result: Float(0), expression: atomLog.Apply(Integer(1)), ok: true},
		{title: "log(1.0)", result: Float(0), expression: atomLog.Apply(Float(1)), ok: true},
		{title: "log(mock)", expression: atomLog.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "log(0.0)", expression: atomLog.Apply(Float(0)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "sqrt(0)", result: Float(0), expression: atomSqrt.Apply(Integer(0)), ok: true},
		{title: "sqrt(0.0)", result: Float(0), expression: atomSqrt.Apply(Float(0)), ok: true},
		{title: "sqrt(mock)", expression: atomSqrt.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "sqrt(-1.0)", result: Float(0), expression: atomSqrt.Apply(Float(-1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "max(1, 2)", result: Integer(2), expression: atomMax.Apply(Integer(1), Integer(2)), ok: true},
		{title: "max(1, 1)", result: Integer(1), expression: atomMax.Apply(Integer(1), Integer(1)), ok: true},
		{title: "max(1, 2.0)", result: Float(2), expression: atomMax.Apply(Integer(1), Float(2)), ok: true},
		{title: "max(1, 1.0)", result: Integer(1), expression: atomMax.Apply(Integer(1), Float(1)), ok: true},
		{title: "max(1, mock)", expression: atomMax.Apply(Integer(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "max(1.0, 2)", result: Integer(2), expression: atomMax.Apply(Float(1), Integer(2)), ok: true},
		{title: "max(1.0, 1)", result: Float(1), expression: atomMax.Apply(Float(1), Integer(1)), ok: true},
		{title: "max(1.0, 2.0)", result: Float(2), expression: atomMax.Apply(Float(1), Float(2)), ok: true},
		{title: "max(1.0, 1.0)", result: Float(1), expression: atomMax.Apply(Float(1), Float(1)), ok: true},
		{title: "max(1.0, mock)", expression: atomMax.Apply(Float(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "max(mock, 1)", expression: atomMax.Apply(&mockNumber{}, Integer(1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "min(2, 1)", result: Integer(1), expression: atomMin.Apply(Integer(2), Integer(1)), ok: true},
		{title: "min(1, 1)", result: Integer(1), expression: atomMin.Apply(Integer(1), Integer(1)), ok: true},
		{title: "min(2, 1.0)", result: Float(1), expression: atomMin.Apply(Integer(2), Float(1)), ok: true},
		{title: "min(1, 1.0)", result: Integer(1), expression: atomMin.Apply(Integer(1), Float(1)), ok: true},
		{title: "min(1, mock)", expression: atomMin.Apply(Integer(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "min(2.0, 1)", result: Integer(1), expression: atomMin.Apply(Float(2), Integer(1)), ok: true},
		{title: "min(1.0, 1)", result: Float(1), expression: atomMin.Apply(Float(1), Integer(1)), ok: true},
		{title: "min(2.0, 1.0)", result: Float(1), expression: atomMin.Apply(Float(2), Float(1)), ok: true},
		{title: "min(1.0, 1.0)", result: Float(1), expression: atomMin.Apply(Float(1), Float(1)), ok: true},
		{title: "min(1.0, mock)", expression: atomMin.Apply(Float(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "min(mock, 1)", expression: atomMin.Apply(&mockNumber{}, Integer(1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "1 ^ 1", result: Integer(1), expression: atomCaret.Apply(Integer(1), Integer(1)), ok: true},
		{title: "1 ^ -1", result: Integer(1), expression: atomCaret.Apply(Integer(1), Integer(-1)), ok: true},
		{title: "0 ^ -1", expression: atomCaret.Apply(Integer(0), Integer(-1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "-1 ^ -1", result: Integer(-1), expression: atomCaret.Apply(Integer(-1), Integer(-1)), ok: true},
		{title: "-1 ^ minInt", expression: atomCaret.Apply(Integer(-1), Integer(math.MinInt64)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "2 ^ -2", expression: atomCaret.Apply(Integer(2), Integer(-2)), err: typeError(validTypeFloat, Integer(2), nil)},
		{title: "1 ^ 1.0", result: Float(1), expression: atomCaret.Apply(Integer(1), Float(1)), ok: true},
		{title: "1 ^ mock", expression: atomCaret.Apply(Integer(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "maxInt ^ 2", expression: atomCaret.Apply(Integer(math.MaxInt64), Integer(2)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "2 ^ 63", expression: atomCaret.Apply(Integer(2), Integer(63)), err: evaluationError(exceptionalValueIntOverflow, nil)},
		{title: "1.0 ^ 1", result: Float(1), expression: atomCaret.Apply(Float(1), Integer(1)), ok: true},
		{title: "1.0 ^ 1.0", result: Float(1), expression: atomCaret.Apply(Float(1), Float(1)), ok: true},
		{title: "1.0 ^ mock", expression: atomCaret.Apply(Float(1), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "mock ^ 1.0", expression: atomCaret.Apply(&mockNumber{}, Float(1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "maxFloat ^ 2.0", expression: atomCaret.Apply(Float(math.MaxFloat64), Float(2)), err: evaluationError(exceptionalValueFloatOverflow, nil)},
		{title: "ε ^ 2.0", expression: atomCaret.Apply(Float(math.SmallestNonzeroFloat64), Float(2)), err: evaluationError(exceptionalValueUnderflow, nil)},
		{title: "-1 ^ 1.1", expression: atomCaret.Apply(Integer(-1), Float(1.1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "0 ^ -1", expression: atomCaret.Apply(Integer(0), Integer(-1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "asin(0)", result: Float(0), expression: atomAsin.Apply(Integer(0)), ok: true},
		{title: "asin(0.0)", result: Float(0), expression: atomAsin.Apply(Float(0)), ok: true},
		{title: "asin(mock)", expression: atomAsin.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "asin(1.1)", expression: atomAsin.Apply(Float(1.1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "asin(-1.1)", expression: atomAsin.Apply(Float(-1.1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "acos(1)", result: Float(0), expression: atomAcos.Apply(Integer(1)), ok: true},
		{title: "acos(0.0)", result: Float(0), expression: atomAcos.Apply(Float(1)), ok: true},
		{title: "acos(mock)", expression: atomAcos.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "acos(1.1)", expression: atomAcos.Apply(Float(1.1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "acos(-1.1)", expression: atomAcos.Apply(Float(-1.1)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "atan2(0, 1)", result: Float(0), expression: atomAtan2.Apply(Integer(0), Integer(1)), ok: true},
		{title: "atan2(0, 1.0)", result: Float(0), expression: atomAtan2.Apply(Integer(0), Float(1)), ok: true},
		{title: "atan2(0, mock)", expression: atomAtan2.Apply(Integer(0), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "atan2(0.0, 1)", result: Float(0), expression: atomAtan2.Apply(Float(0), Integer(1)), ok: true},
		{title: "atan2(0.0, 1.0)", result: Float(0), expression: atomAtan2.Apply(Float(0), Float(1)), ok: true},
		{title: "atan2(0.0, mock)", expression: atomAtan2.Apply(Float(0), &mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "atan2(mock, 1)", expression: atomAtan2.Apply(&mockNumber{}, Integer(1)), err: evaluationError(exceptionalValueUndefined, nil)},
		{title: "atan2(0, 0)", expression: atomAtan2.Apply(Integer(0), Integer(0)), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "tan(0)", result: Float(0), expression: atomTan.Apply(Integer(0)), ok: true},
		{title: "tan(0.0)", result: Float(0), expression: atomTan.Apply(Float(0)), ok: true},
		{title: "tan(mock)", expression: atomTan.Apply(&mockNumber{}), err: evaluationError(exceptionalValueUndefined, nil)},

		{title: "16 >> 2", result: Integer(4), expression: atomBitwiseRightShift.Apply(Integer(16), Integer(2)), ok: true},
		{title: "16 >> 2.0", expression: atomBitwiseRightShift.Apply(Integer(16), Float(2)), err: typeError(validTypeInteger, Float(2), nil)},
		{title: "16.0 >> 2", expression: atomBitwiseRightShift.Apply(Float(16), Integer(2)), err: typeError(validTypeInteger, Float(16), nil)},

		{title: "16 << 2", result: Integer(64), expression: atomBitwiseLeftShift.Apply(Integer(16), Integer(2)), ok: true},
		{title: "16 << 2.0", expression: atomBitwiseLeftShift.Apply(Integer(16), Float(2)), err: typeError(validTypeInteger, Float(2), nil)},
		{title: "16.0 << 2", expression: atomBitwiseLeftShift.Apply(Float(16), Integer(2)), err: typeError(validTypeInteger, Float(16), nil)},

		{title: `10 /\ 12`, result: Integer(8), expression: atomBitwiseAnd.Apply(Integer(10), Integer(12)), ok: true},
		{title: `10 /\ 12.0`, expression: atomBitwiseAnd.Apply(Integer(10), Float(12)), err: typeError(validTypeInteger, Float(12), nil)},
		{title: `10.0 /\ 12`, expression: atomBitwiseAnd.Apply(Float(10), Integer(12)), err: typeError(validTypeInteger, Float(10), nil)},

		{title: `10 \/ 12`, result: Integer(14), expression: atomBitwiseOr.Apply(Integer(10), Integer(12)), ok: true},
		{title: `10 \/ 12.0`, expression: atomBitwiseOr.Apply(Integer(10), Float(12)), err: typeError(validTypeInteger, Float(12), nil)},
		{title: `10.0 \/ 12`, expression: atomBitwiseOr.Apply(Float(10), Integer(12)), err: typeError(validTypeInteger, Float(10), nil)},

		{title: `\ \ 10`, result: Integer(10), expression: atomBackSlash.Apply(atomBackSlash.Apply(Integer(10))), ok: true},
		{title: `\ \ 10.0`, expression: atomBackSlash.Apply(atomBackSlash.Apply(Float(10))), err: typeError(validTypeInteger, Float(10), nil)},

		{title: "xor(10, 12)", result: Integer(6), expression: atomXor.Apply(Integer(10), Integer(12)), ok: true},
		{title: "xor(10, 12.0)", expression: atomXor.Apply(Integer(10), Float(12)), err: typeError(validTypeInteger, Float(12), nil)},
		{title: "xor(10.0, 12)", expression: atomXor.Apply(Float(10), Integer(12)), err: typeError(validTypeInteger, Float(10), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			var vm internal.VM
			ok, err := Is(&vm, tt.result, tt.expression, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestEqual(t *testing.T) {
	var vm internal.VM
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := Equal(&vm, Integer(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := Equal(&vm, Integer(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := Equal(&vm, Float(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := Equal(&vm, Float(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("e1 is a variable", func(t *testing.T) {
		_, err := Equal(&vm, Integer(1), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("e2 is a variable", func(t *testing.T) {
		_, err := Equal(&vm, internal.NewVariable(), Integer(1), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("ng", func(t *testing.T) {
		ok, err := Equal(&vm, Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestNotEqual(t *testing.T) {
	x := internal.NewVariable()

	tests := []struct {
		title  string
		e1, e2 Term
		ok     bool
		err    error
	}{
		{title: `1 =\= 2`, e1: Integer(1), e2: Integer(2), ok: true},
		{title: `1 =\= 2.0`, e1: Integer(1), e2: Float(2), ok: true},
		{title: `1.0 =\= 2`, e1: Float(1), e2: Integer(2), ok: true},
		{title: `1.0 =\= 2.0`, e1: Float(1), e2: Float(2), ok: true},
		{title: `X =\= 1`, e1: x, e2: Integer(1), err: InstantiationError(nil)},
		{title: `1 =\= X`, e1: Integer(1), e2: x, err: InstantiationError(nil)},
		{title: `1 =\= 1`, e1: Integer(1), e2: Integer(1), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := NotEqual(nil, tt.e1, tt.e2, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestLessThan(t *testing.T) {
	x := internal.NewVariable()

	tests := []struct {
		title  string
		e1, e2 Term
		ok     bool
		err    error
	}{
		{title: `1 < 2`, e1: Integer(1), e2: Integer(2), ok: true},
		{title: `1 < 2.0`, e1: Integer(1), e2: Float(2), ok: true},
		{title: `1.0 < 2`, e1: Float(1), e2: Integer(2), ok: true},
		{title: `1.0 < 2.0`, e1: Float(1), e2: Float(2), ok: true},
		{title: `X < 1`, e1: x, e2: Integer(1), err: InstantiationError(nil)},
		{title: `1 < X`, e1: Integer(1), e2: x, err: InstantiationError(nil)},
		{title: `1 < 1`, e1: Integer(1), e2: Integer(1), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := LessThan(nil, tt.e1, tt.e2, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestGreaterThan(t *testing.T) {
	x := internal.NewVariable()

	tests := []struct {
		title  string
		e1, e2 Term
		ok     bool
		err    error
	}{
		{title: `2 > 1`, e1: Integer(2), e2: Integer(1), ok: true},
		{title: `2 > 1.0`, e1: Integer(2), e2: Float(1), ok: true},
		{title: `2.0 > 1`, e1: Float(2), e2: Integer(1), ok: true},
		{title: `2.0 > 1.0`, e1: Float(2), e2: Float(1), ok: true},
		{title: `X > 1`, e1: x, e2: Integer(1), err: InstantiationError(nil)},
		{title: `1 > X`, e1: Integer(1), e2: x, err: InstantiationError(nil)},
		{title: `1 > 1`, e1: Integer(1), e2: Integer(1), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := GreaterThan(nil, tt.e1, tt.e2, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestLessThanOrEqual(t *testing.T) {
	x := internal.NewVariable()

	tests := []struct {
		title  string
		e1, e2 Term
		ok     bool
		err    error
	}{
		{title: `1 =< 1`, e1: Integer(1), e2: Integer(1), ok: true},
		{title: `1 =< 1.0`, e1: Integer(1), e2: Float(1), ok: true},
		{title: `1.0 =< 1`, e1: Float(1), e2: Integer(1), ok: true},
		{title: `1.0 =< 1.0`, e1: Float(1), e2: Float(1), ok: true},
		{title: `X =< 1`, e1: x, e2: Integer(1), err: InstantiationError(nil)},
		{title: `1 =< X`, e1: Integer(1), e2: x, err: InstantiationError(nil)},
		{title: `2 =< 1`, e1: Integer(2), e2: Integer(1), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := LessThanOrEqual(nil, tt.e1, tt.e2, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestGreaterThanOrEqual(t *testing.T) {
	x := internal.NewVariable()

	tests := []struct {
		title  string
		e1, e2 Term
		ok     bool
		err    error
	}{
		{title: `1 >= 1`, e1: Integer(1), e2: Integer(1), ok: true},
		{title: `1 >= 1.0`, e1: Integer(1), e2: Float(1), ok: true},
		{title: `1.0 >= 1`, e1: Float(1), e2: Integer(1), ok: true},
		{title: `1.0 >= 1.0`, e1: Float(1), e2: Float(1), ok: true},
		{title: `X >= 1`, e1: x, e2: Integer(1), err: InstantiationError(nil)},
		{title: `1 >= X`, e1: Integer(1), e2: x, err: InstantiationError(nil)},
		{title: `1 >= 2`, e1: Integer(1), e2: Integer(2), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := GreaterThanOrEqual(nil, tt.e1, tt.e2, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

type mockNumber struct {
	mock.Mock
}

func (m *mockNumber) number() {
	_ = m.Called()
}

func (m *mockNumber) WriteTerm(w io.Writer, opts *internal.WriteOptions, env *internal.Env) error {
	args := m.Called(w, opts, env)
	return args.Error(0)
}

func (m *mockNumber) Compare(t Term, env *internal.Env) int {
	args := m.Called(t, env)
	return args.Int(0)
}
