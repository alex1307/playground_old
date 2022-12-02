package artillery

import (
	"balistic-engine/pkg/math"

	"github.com/google/uuid"
)

type Missle struct {
	ID       string
	Velocity float64
	Radians  float64
	Source   math.Coordinates
	Target   math.Coordinates
}

func NewMissle(velocity float64) *Missle {
	missle := &Missle{ID: uuid.Must(uuid.NewRandom()).String(), Velocity: velocity}
	return missle
}
