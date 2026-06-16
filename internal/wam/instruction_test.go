package wam

import (
	"testing"
	"unsafe"
)

func TestInstruction_size(t *testing.T) {
	if size := unsafe.Sizeof(Instruction{}); size != 4 {
		t.Fatalf("instruction must be 4 bytes: %d", size)
	}
}
