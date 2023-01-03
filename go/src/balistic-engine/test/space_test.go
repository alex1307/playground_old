package test

import (
	"balistic-engine/pkg/config"
	"balistic-engine/pkg/math"
	"balistic-engine/pkg/monitor"
	"balistic-engine/pkg/projectile"
	"testing"
	"time"

	"github.com/google/uuid"
	"github.com/stretchr/testify/assert"
)

var TimeSeries map[int][]projectile.TrajectoryCoordinates

func TestCoordinatesToRectangleSeed(t *testing.T) {
	config.Setup()
	seed := 10.0
	start := math.Coordinates{X: 0, Y: 0, T: 0}
	end := math.Coordinates{X: 10.0, Y: 10.0, T: 10.0}
	lenght := end.X - start.X
	height := end.Y - start.Y
	height_seed := height / seed
	lenght_seed := lenght / seed
	coordinate := math.Coordinates{X: 9.9, Y: 9.1, T: 0}
	x_index := int(coordinate.X/lenght_seed) + 1
	y_index := int(coordinate.Y / height_seed)
	assert.Equal(t, 100, y_index*int(seed)+x_index)
}

func TestOneProjectile(t *testing.T) {
	config.Setup()
	team_ua := "Ukraine"
	space := monitor.NewSpace(10.0, [2]math.Coordinates{
		{X: 0, Y: 0, T: 0},
		{X: 10.0, Y: 10.0, T: 10.0},
	})
	projectile_id := uuid.NewString()
	config.AppLogger.Info("Projectile ID: " + projectile_id)
	space.Move(projectile.TrajectoryCoordinates{
		ID:          projectile_id,
		TeamID:      team_ua,
		SystemTime:  time.Now().Unix() * 1000,
		Coordinates: math.Coordinates{X: 1.0, Y: 1.0, T: 0},
	})

	index := space.GetIndex(projectile_id)
	assert.Equal(t, 12, index)
	assert.Equal(t, 1, len(space.GetActiveProjectiles(index)))

}

func TestSpaceTrajectories(t *testing.T) {
	config.Setup()
	team_ua := "Ukraine"
	team_rs := "Russia"
	space := monitor.NewSpace(10.0, [2]math.Coordinates{
		{X: 0, Y: 0, T: 0},
		{X: 10.0, Y: 10.0, T: 10.0},
	})
	p1 := projectile.TrajectoryCoordinates{
		ID:          uuid.NewString(),
		TeamID:      team_ua,
		SystemTime:  time.Now().Unix() * 1000,
		Coordinates: math.Coordinates{X: 1.0, Y: 1.0, T: 0},
	}
	//Move projectile #1 from index 12 [1,1] to index 100 [9.5,9.1]
	space.Move(p1)
	index := space.GetIndex(p1.ID)
	assert.Equal(t, 12, index)
	assert.Equal(t, 1, len(space.GetActiveProjectiles(index)))

	//Change position and move it on the monitor
	p1.Coordinates.X = 9.5
	p1.Coordinates.Y = 9.1
	space.Move(p1)
	index = space.GetIndex(p1.ID)
	assert.Equal(t, 100, index)
	assert.Equal(t, 1, len(space.GetActiveProjectiles(index)))
	assert.Equal(t, 0, len(space.GetActiveProjectiles(12)))

	p2 := projectile.TrajectoryCoordinates{
		ID:          uuid.NewString(),
		TeamID:      team_ua,
		SystemTime:  time.Now().Unix() * 1000,
		Coordinates: math.Coordinates{X: 9.3, Y: 9.4, T: 0},
	}

	space.Move(p2)
	assert.Equal(t, 100, space.GetIndex(p1.ID))
	assert.Equal(t, 100, space.GetIndex(p2.ID))
	assert.Equal(t, 2, len(space.GetActiveProjectiles(index)))
	p3 := projectile.TrajectoryCoordinates{
		ID:          uuid.NewString(),
		TeamID:      team_rs,
		SystemTime:  time.Now().Unix() * 1000,
		Coordinates: math.Coordinates{X: 9.6, Y: 9.7, T: 0},
	}
	space.Move(p3)
	assert.Equal(t, 1, len(space.GetActiveProjectiles(index)))
	p4 := projectile.TrajectoryCoordinates{
		ID:          uuid.NewString(),
		TeamID:      team_rs,
		SystemTime:  time.Now().Unix() * 1000,
		Coordinates: math.Coordinates{X: 9.6, Y: 9.7, T: 0},
	}
	space.Move(p4)
	assert.Equal(t, 0, len(space.GetActiveProjectiles(index)))
}

func TestOutOfMonitoredSpace(t *testing.T) {
	config.Setup()
	team_ua := "Ukraine"
	space := monitor.NewSpace(10.0, [2]math.Coordinates{
		{X: 0, Y: 0, T: 0},
		{X: 10.0, Y: 10.0, T: 10.0},
	})
	projectile_id := uuid.NewString()
	config.AppLogger.Info("Projectile ID: " + projectile_id)
	space.Move(projectile.TrajectoryCoordinates{
		ID:          projectile_id,
		TeamID:      team_ua,
		SystemTime:  time.Now().Unix() * 1000,
		Coordinates: math.Coordinates{X: 11.0, Y: 11.0, T: 0},
	})

	index := space.GetIndex(projectile_id)
	assert.Equal(t, 122, index)
	assert.Equal(t, 1, len(space.GetActiveProjectiles(index)))

}
