package engine

import (
	"errors"
	"runtime"
	"runtime/debug"
	"unsafe"
)

var errOutOfMemory = errors.New("out of memory")

var memFree = func() int64 {
	limit := debug.SetMemoryLimit(-1)
	var stats runtime.MemStats
	runtime.ReadMemStats(&stats)
	return limit - int64(stats.Sys-stats.HeapReleased)
}

// makeSlice tries to allocate a slice safely by respecting debug.SetMemoryLimit().
// There's still a chance to breach the limit due to a race condition.
// Yet, it can still prevent allocation of unreasonably large slices.
func makeSlice[T any](n int) (_ []T, err error) {
	defer func() {
		if r := recover(); r != nil {
			// e.g. "runtime error: makeslice: len out of range"
			err = errOutOfMemory
		}
	}()

	free := memFree()

	var t T
	size := int64(unsafe.Sizeof(t))

	if free < int64(n)*size {
		return nil, errOutOfMemory
	}

	return make([]T, n), nil
}
