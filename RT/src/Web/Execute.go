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

package Web

import (
	"net/http"

	"CRW/RT/Display"
	"CRW/RT/Program"
)

type Execute struct {
	Programs Programs
}

func (e *Execute) ServeHTTP(res http.ResponseWriter, req *http.Request) {
	p, ok := e.Programs[req.URL.Path]
	if !ok {
		res.WriteHeader(http.StatusNotFound)
		return
	}
	err := e.executeProgram(res, req, p)
	if err != nil {
		res.WriteHeader(http.StatusInternalServerError)
		return
	}
}

func (e *Execute) executeProgram(res http.ResponseWriter, req *http.Request, p Program.Program) error {
	switch p := p.(type) {
	case *Program.View:
		return e.executeView(res, req, p)
	case *Program.Action:
		return e.executeAction(res, req, p)
	default:
		panic("Invalid program type")
	}
}

func (e *Execute) executeView(res http.ResponseWriter, req *http.Request, p *Program.View) error {
	display := e.getDisplay(res)
	err := p.Execute(display)
	if err != nil {
		return err
	}
	return display.Flush()
}

func (e *Execute) executeAction(res http.ResponseWriter, req *http.Request, p *Program.Action) error {
	panic("Not yet implemented")
}

func (e *Execute) getDisplay(res http.ResponseWriter) display.Display {
	return display.NewCSVDisplay(res)
}
