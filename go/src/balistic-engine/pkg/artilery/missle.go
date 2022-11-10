package artilery

import (
	"github.com/google/uuid"
)

type Missle struct {
	ID       string
	Velocity float64
}

func NewMissle(velocity float64) *Missle {
	missle := &Missle{ID: uuid.Must(uuid.NewRandom()).String(), Velocity: velocity}
	return missle
}
