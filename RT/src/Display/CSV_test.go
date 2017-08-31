// Copyright 2017, rightfold
//
// This file is part of CRW.
//
// CRW is free software: you can redistribute it and/or modify it under the
// terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// CRW is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License
// along with CRW. If not, see <http://www.gnu.org/licenses/>.

package display

import (
	"bytes"
	"testing"
)

func TestCSVDisplay(t *testing.T) {
	var buffer bytes.Buffer
	display := NewCSVDisplay(&buffer)
	display.Display(1, 2, 3)
	display.Display(4, 5, 6)
	display.Flush()
	if buffer.String() != "1,2,3\n4,5,6\n" {
		t.Errorf("Did not write correct CSV output: %s", buffer.String())
	}
}
