package wam

import (
	"testing"
	"unsafe"
)

func TestInstruction_size(t *testing.T) {
	if size := unsafe.Sizeof(Instruction{}); size != 8 {
		t.Fatalf("instruction must be 8 bytes: %d", size)
	}
}
