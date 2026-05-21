package term

import (
	"testing"
	"unsafe"
)

func TestCell_size(t *testing.T) {
	if unsafe.Sizeof(cell{}) != 8 {
		t.Fatal("cell must be 8 bytes")
	}
}
