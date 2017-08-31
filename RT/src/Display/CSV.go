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
	"encoding/csv"
	"fmt"
	"io"
)

type CSVDisplay struct {
	w *csv.Writer
}

func NewCSVDisplay(w io.Writer) *CSVDisplay {
	return &CSVDisplay{w: csv.NewWriter(w)}
}

func (d *CSVDisplay) Display(cells ...interface{}) error {
	record := make([]string, len(cells))
	for i, cell := range cells {
		record[i] = fmt.Sprint(cell)
	}
	return d.w.Write(record)
}

func (d *CSVDisplay) Flush() error {
	d.w.Flush()
	return d.w.Error()
}
