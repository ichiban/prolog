package engine

import (
	"fmt"
	"io"
	"regexp"
	"strings"
	"sync"
	"unicode/utf8"
)

var (
	quotedAtomEscapePattern = regexp.MustCompile(`[[:cntrl:]]|\\|'`)
)

var (
	atomTable = struct {
		sync.RWMutex
		names []string
		atoms map[string]Atom
	}{
		atoms: map[string]Atom{},
	}
)

// Well-known atoms.
var (
	atomEmpty             = NewAtom("")
	atomSlash             = NewAtom("/")
	atomSlashSlash        = NewAtom("//")
	atomColonMinus        = NewAtom(":-")
	atomEmptyList         = NewAtom("[]")
	atomEmptyBlock        = NewAtom("{}")
	atomPlus              = NewAtom("+")
	atomMinus             = NewAtom("-")
	atomAsterisk          = NewAtom("*")
	atomAsteriskAsterisk  = NewAtom("**")
	atomLessThan          = NewAtom("<")
	atomEqual             = NewAtom("=")
	atomGreaterThan       = NewAtom(">")
	atomDot               = NewAtom(".")
	atomComma             = NewAtom(",")
	atomBar               = NewAtom("|")
	atomCut               = NewAtom("!")
	atomColon             = NewAtom(":")
	atomSemiColon         = NewAtom(";")
	atomNegation          = NewAtom(`\+`)
	atomThen              = NewAtom("->")
	atomCaret             = NewAtom("^")
	atomArrow             = NewAtom("-->")
	atomBackSlash         = NewAtom(`\`)
	atomBitwiseRightShift = NewAtom(">>")
	atomBitwiseLeftShift  = NewAtom("<<")
	atomBitwiseAnd        = NewAtom(`/\`)
	atomBitwiseOr         = NewAtom(`\/`)
	atomElipsis           = NewAtom(`...`)
	atomQuestionMinus     = NewAtom("?-")

	atomAbs                     = NewAtom("abs")
	atomAccess                  = NewAtom("access")
	atomAcos                    = NewAtom("acos")
	atomAlias                   = NewAtom("alias")
	atomAll                     = NewAtom("all")
	atomAppend                  = NewAtom("append")
	atomAsin                    = NewAtom("asin")
	atomAt                      = NewAtom("at")
	atomAtan                    = NewAtom("atan")
	atomAtan2                   = NewAtom("atan2")
	atomAtom                    = NewAtom("atom")
	atomAtomic                  = NewAtom("atomic")
	atomBinary                  = NewAtom("binary")
	atomBinaryStream            = NewAtom("binary_stream")
	atomBounded                 = NewAtom("bounded")
	atomByte                    = NewAtom("byte")
	atomCall                    = NewAtom("call")
	atomCallable                = NewAtom("callable")
	atomCeiling                 = NewAtom("ceiling")
	atomChanged                 = NewAtom("changed")
	atomCharConversion          = NewAtom("char_conversion")
	atomCharacter               = NewAtom("character")
	atomCharacterCode           = NewAtom("character_code")
	atomCharacterCodeList       = NewAtom("character_code_list")
	atomChars                   = NewAtom("chars")
	atomCloseOption             = NewAtom("close_option")
	atomCodes                   = NewAtom("codes")
	atomCompound                = NewAtom("compound")
	atomCos                     = NewAtom("cos")
	atomCreate                  = NewAtom("create")
	atomDebug                   = NewAtom("debug")
	atomDiscontiguous           = NewAtom("discontiguous")
	atomDiv                     = NewAtom("div")
	atomDomainError             = NewAtom("domain_error")
	atomDoubleQuotes            = NewAtom("double_quotes")
	atomDynamic                 = NewAtom("dynamic")
	atomE                       = NewAtom("E")
	atomEOFAction               = NewAtom("eof_action")
	atomEOFCode                 = NewAtom("eof_code")
	atomEndOfFile               = NewAtom("end_of_file")
	atomEndOfStream             = NewAtom("end_of_stream")
	atomEnsureLoaded            = NewAtom("ensure_loaded")
	atomError                   = NewAtom("error")
	atomEvaluable               = NewAtom("evaluable")
	atomEvaluationError         = NewAtom("evaluation_error")
	atomExistenceError          = NewAtom("existence_error")
	atomExp                     = NewAtom("exp")
	atomFX                      = NewAtom("fx")
	atomFY                      = NewAtom("fy")
	atomFail                    = NewAtom("fail")
	atomFalse                   = NewAtom("false")
	atomFileName                = NewAtom("file_name")
	atomFiniteMemory            = NewAtom("finite_memory")
	atomFlag                    = NewAtom("flag")
	atomFlagValue               = NewAtom("flag_value")
	atomFloat                   = NewAtom("float")
	atomFloatFractionalPart     = NewAtom("float_fractional_part")
	atomFloatIntegerPart        = NewAtom("float_integer_part")
	atomFloatOverflow           = NewAtom("float_overflow")
	atomFloor                   = NewAtom("floor")
	atomForce                   = NewAtom("force")
	atomIf                      = NewAtom("if")
	atomIgnoreOps               = NewAtom("ignore_ops")
	atomImports                 = NewAtom("imports")
	atomInByte                  = NewAtom("in_byte")
	atomInCharacter             = NewAtom("in_character")
	atomInCharacterCode         = NewAtom("in_character_code")
	atomInclude                 = NewAtom("include")
	atomInitialization          = NewAtom("initialization")
	atomInput                   = NewAtom("input")
	atomInstantiationError      = NewAtom("instantiation_error")
	atomIntOverflow             = NewAtom("int_overflow")
	atomInteger                 = NewAtom("integer")
	atomIntegerRoundingFunction = NewAtom("integer_rounding_function")
	atomIOMode                  = NewAtom("io_mode")
	atomList                    = NewAtom("list")
	atomLog                     = NewAtom("log")
	atomMax                     = NewAtom("max")
	atomMaxArity                = NewAtom("max_arity")
	atomMaxDepth                = NewAtom("max_depth")
	atomMaxInteger              = NewAtom("max_integer")
	atomMemory                  = NewAtom("memory")
	atomMetaArgumentSpecifier   = NewAtom("meta_argument_specifier")
	atomMin                     = NewAtom("min")
	atomMinInteger              = NewAtom("min_integer")
	atomMod                     = NewAtom("mod")
	atomMode                    = NewAtom("mode")
	atomModify                  = NewAtom("modify")
	atomMultifile               = NewAtom("multifile")
	atomNonEmptyList            = NewAtom("non_empty_list")
	atomNot                     = NewAtom("not")
	atomNotLessThanZero         = NewAtom("not_less_than_zero")
	atomNumber                  = NewAtom("number")
	atomNumberVars              = NewAtom("numbervars")
	atomOff                     = NewAtom("off")
	atomOn                      = NewAtom("on")
	atomOp                      = NewAtom("op")
	atomOpen                    = NewAtom("open")
	atomOperator                = NewAtom("operator")
	atomOperatorPriority        = NewAtom("operator_priority")
	atomOperatorSpecifier       = NewAtom("operator_specifier")
	atomOrder                   = NewAtom("order")
	atomOutput                  = NewAtom("output")
	atomPair                    = NewAtom("pair")
	atomPast                    = NewAtom("past")
	atomPastEndOfStream         = NewAtom("past_enf_of_stream")
	atomPermissionError         = NewAtom("permission_error")
	atomPhrase                  = NewAtom("phrase")
	atomPi                      = NewAtom("pi")
	atomPosition                = NewAtom("position")
	atomPredicateIndicator      = NewAtom("predicate_indicator")
	atomPrivateProcedure        = NewAtom("private_procedure")
	atomProcedure               = NewAtom("procedure")
	atomProlog                  = NewAtom("prolog")
	atomPrologFlag              = NewAtom("prolog_flag")
	atomQuoted                  = NewAtom("quoted")
	atomRead                    = NewAtom("read")
	atomReadOption              = NewAtom("read_option")
	atomRem                     = NewAtom("rem")
	atomReposition              = NewAtom("reposition")
	atomRepresentationError     = NewAtom("representation_error")
	atomReset                   = NewAtom("reset")
	atomResourceError           = NewAtom("resource_error")
	atomRound                   = NewAtom("round")
	atomSign                    = NewAtom("sign")
	atomSin                     = NewAtom("sin")
	atomSingletons              = NewAtom("singletons")
	atomSmallE                  = NewAtom("e")
	atomSourceSink              = NewAtom("source_sink")
	atomSqrt                    = NewAtom("sqrt")
	atomStaticProcedure         = NewAtom("static_procedure")
	atomStream                  = NewAtom("stream")
	atomStreamOption            = NewAtom("stream_option")
	atomStreamOrAlias           = NewAtom("stream_or_alias")
	atomStreamPosition          = NewAtom("stream_position")
	atomStreamProperty          = NewAtom("stream_property")
	atomSyntaxError             = NewAtom("syntax_error")
	atomTan                     = NewAtom("tan")
	atomTermExpansion           = NewAtom("term_expansion")
	atomText                    = NewAtom("text")
	atomTextStream              = NewAtom("text_stream")
	atomTowardZero              = NewAtom("toward_zero")
	atomTrue                    = NewAtom("true")
	atomTruncate                = NewAtom("truncate")
	atomType                    = NewAtom("type")
	atomTypeError               = NewAtom("type_error")
	atomUnbounded               = NewAtom("unbounded")
	atomUndefined               = NewAtom("undefined")
	atomUnderflow               = NewAtom("underflow")
	atomUnknown                 = NewAtom("unknown")
	atomUser                    = NewAtom("user")
	atomUserInput               = NewAtom("user_input")
	atomUserOutput              = NewAtom("user_output")
	atomVar                     = NewAtom("$VAR")
	atomVariableNames           = NewAtom("variable_names")
	atomVariables               = NewAtom("variables")
	atomWarning                 = NewAtom("warning")
	atomWrite                   = NewAtom("write")
	atomWriteOption             = NewAtom("write_option")
	atomXF                      = NewAtom("xf")
	atomXFX                     = NewAtom("xfx")
	atomXFY                     = NewAtom("xfy")
	atomXor                     = NewAtom("xor")
	atomYF                      = NewAtom("yf")
	atomYFX                     = NewAtom("yfx")
	atomZeroDivisor             = NewAtom("zero_divisor")
)

// Atom is a prolog atom.
type Atom uint64

// NewAtom interns the given string and returns an Atom.
func NewAtom(name string) Atom {
	// A one-char atom is just a rune.
	if r, n := utf8.DecodeLastRuneInString(name); r != utf8.RuneError && n == len(name) {
		return Atom(r)
	}

	atomTable.Lock()
	defer atomTable.Unlock()

	a, ok := atomTable.atoms[name]
	if ok {
		return a
	}

	a = Atom(len(atomTable.names) + (utf8.MaxRune + 1))
	atomTable.atoms[name] = a
	atomTable.names = append(atomTable.names, name)
	return a
}

// WriteTerm outputs the Atom to an io.Writer.
func (a Atom) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	ew := errWriter{w: w}
	openClose := (opts.left != (operator{}) || opts.right != (operator{})) && opts.ops.defined(a)

	if openClose {
		if opts.left.name != 0 && opts.left.specifier.class() == operatorClassPrefix {
			_, _ = ew.Write([]byte(" "))
		}
		_, _ = ew.Write([]byte("("))
		opts = opts.withLeft(operator{}).withRight(operator{})
	}

	if opts.quoted && needQuoted(a) {
		if opts.left != (operator{}) && needQuoted(opts.left.name) { // Avoid 'FOO''BAR'.
			_, _ = ew.Write([]byte(" "))
		}
		_, _ = ew.Write([]byte(quote(a.String())))
		if opts.right != (operator{}) && needQuoted(opts.right.name) { // Avoid 'FOO''BAR'.
			_, _ = ew.Write([]byte(" "))
		}
	} else {
		if (letterDigit(opts.left.name) && letterDigit(a)) || (graphic(opts.left.name) && graphic(a)) {
			_, _ = ew.Write([]byte(" "))
		}
		_, _ = ew.Write([]byte(a.String()))
		if (letterDigit(opts.right.name) && letterDigit(a)) || (graphic(opts.right.name) && graphic(a)) {
			_, _ = ew.Write([]byte(" "))
		}
	}

	if openClose {
		_, _ = ew.Write([]byte(")"))
	}

	return ew.err
}

// Compare compares the Atom with a Term.
func (a Atom) Compare(t Term, env *Env) int {
	switch t := env.Resolve(t).(type) {
	case Variable, Float, Integer:
		return 1
	case Atom:
		switch d := strings.Compare(a.String(), t.String()); {
		case d > 0:
			return 1
		case d < 0:
			return -1
		default:
			return 0
		}
	default: // Custom atomic terms, Compound.
		return -1
	}
}

func (a Atom) String() string {
	if a <= utf8.MaxRune {
		return string(rune(a))
	}
	atomTable.RLock()
	defer atomTable.RUnlock()
	return atomTable.names[a-(utf8.MaxRune+1)]
}

// Apply returns a Compound which Functor is the Atom and args are the arguments. If the arguments are empty,
// then returns itself.
func (a Atom) Apply(args ...Term) Term {
	if len(args) == 0 {
		return a
	}
	return &compound{
		functor: a,
		args:    args,
	}
}

func needQuoted(a Atom) bool {
	p := Parser{
		Lexer: Lexer{
			input: newRuneRingBuffer(strings.NewReader(a.String())),
		},
	}
	parsed, err := p.atom()
	return err != nil || parsed != a
}

func quote(s string) string {
	return fmt.Sprintf("'%s'", quotedAtomEscapePattern.ReplaceAllStringFunc(s, quotedIdentEscape))
}

func quotedIdentEscape(s string) string {
	switch s {
	case "\a":
		return `\a`
	case "\b":
		return `\b`
	case "\f":
		return `\f`
	case "\n":
		return `\n`
	case "\r":
		return `\r`
	case "\t":
		return `\t`
	case "\v":
		return `\v`
	case `\`:
		return `\\`
	case `'`:
		return `\'`
	default:
		var ret []string
		for _, r := range s {
			ret = append(ret, fmt.Sprintf(`\x%x\`, r))
		}
		return strings.Join(ret, "")
	}
}

func letterDigit(a Atom) bool {
	s := a.String()
	return len(s) > 0 && isSmallLetterChar([]rune(s)[0])
}

func graphic(a Atom) bool {
	s := a.String()
	return len(s) > 0 && (isGraphicChar([]rune(s)[0]) || []rune(s)[0] == '\\')
}
