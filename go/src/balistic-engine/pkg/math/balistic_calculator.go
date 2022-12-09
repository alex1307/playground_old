package math

import (
	"fmt"
	"math"
)

const G_FORCE = 9.80665

func Position(position Coordinates,
	velocity,
	radians,
	time float64) (Coordinates, error) {
	//x = cos(Θ)•V•t
	//y = h + sin(Θ)•V•t - ½gt²

	cos_angel := math.Cos(radians)
	sin_angle := math.Sin(radians)
	x := position.X + cos_angel*velocity*time
	y := (position.Y + (sin_angle * velocity * time)) - 0.5*G_FORCE*math.Pow(time, 2.0)
	return Coordinates{X: x, Y: y, T: time}, nil
}

// Calculate elapsed projection time
// (V *sin(θ) + SQRT(V * sin(θ) + 2 * G * h))/G
func ElapsedTime(position Coordinates, velocity, radians float64) (float64, error) {

	if velocity <= 0 {
		return 0.0, fmt.Errorf("invalid velocity param! Velocity must be a positive value m/s")
	}

	if position.Y < 0 {
		return 0.0, fmt.Errorf("invalid parameter! The height is a positive number in meters")
	}

	v_sin := velocity * math.Sin(radians)
	attr := 2 * G_FORCE * position.Y
	result := math.Pow(v_sin, 2.0) + attr
	Time := (v_sin + math.Sqrt(result)) / G_FORCE
	return Time, nil
}

func MaxRange(position Coordinates, elapsed_time, velocity, radians float64) (float64, error) {
	return position.X + elapsed_time*velocity*math.Cos(radians), nil
}

// Max = h + ((V * V * sin(θ) * sin(θ))/2 * G)
func MaxAltitude(position Coordinates, velocity, radians float64) (float64, error) {
	if velocity <= 0 {
		return 0.0, fmt.Errorf("invalid velocity param! The velocity must be a positive value m/s")
	}
	if position.Y < 0 {
		return 0.0, fmt.Errorf("invalid parameter! The height is a positive number in meters")
	}
	Velocity_Y := velocity * math.Sin(radians)
	Velocity_2 := math.Pow(Velocity_Y, 2)
	Height_2 := 2 * G_FORCE
	return position.Y + (Velocity_2 / Height_2), nil

}

func Velocity(velocity, radians, time float64) (Coordinates, error) {
	Velocity_X := velocity * math.Cos(radians)
	Velocity_Y := velocity*math.
		Sin(radians) - G_FORCE*time
	return Coordinates{X: Velocity_X, Y: Velocity_Y}, nil
}

func DegreesToRadians(degree float64) (float64, error) {

	if degree < 0 || degree > 90 {
		return 0.0, fmt.Errorf("invalid parameter! Valid degree is in range (0, 90)")
	}

	return (degree * math.Pi) / 180, nil

}

func RadiansToDegrees(radians float64) (float64, error) {

	if radians < 0 || radians > math.Pi {
		return 0.0, fmt.Errorf("invalid parameter! Valid radians are in range (0, 3.14)")
	}

	return (180 * radians) / math.Pi, nil

}

func TimeInterval(elapsed_time float64, seed int) ([]float64, error) {
	if elapsed_time <= 0 {
		return nil, fmt.Errorf("")
	}
	result := make([]float64, seed+1)
	for i := 0; i < seed; i++ {
		result[i] = float64(i) * elapsed_time / float64(seed)
	}
	result[seed] = elapsed_time
	return result, nil
}

func CalculateTrajection(position Coordinates, velocity, radians float64, seed int) ([]Coordinates, error) {

	max_time, err := ElapsedTime(position, velocity, radians)
	if err != nil {
		return nil, err
	}

	intervals, err := TimeInterval(max_time, seed)
	if err != nil {
		return nil, err
	}
	trajection := make([]Coordinates, len(intervals))
	for i, time := range intervals {
		position, errp := Position(position, velocity, radians, time)

		if errp != nil {
			return nil, errp
		}

		trajection[i] = position
	}
	return trajection, nil
}

func PositionToIndex(position, grid_initial_position Coordinates, grid_width, grid_height float64, grid_scale int) (int, error) {
	//grid := field.Grid
	length := position.X - grid_initial_position.X
	height := position.Y - grid_initial_position.Y

	if length < 0 || height < 0 || length > grid_width || height > grid_height {
		return -1, fmt.Errorf("coordinates are out of range")
	}

	step_x := grid_width / float64(grid_scale)
	step_y := grid_height / float64(grid_scale)
	x_index := int(length / step_x)
	y_index := int(height / step_y)

	return x_index*grid_scale + y_index, nil
}

// func CalculateTrajectionToGrid(velocity, degrees, height float64, seed int, grid model.Grid) ([]int, error) {
// 	trajection, err := CalculateTrajection(velocity, degrees, height, seed)
// 	if err != nil {
// 		return nil, err
// 	}
// 	result := make([]int, len(trajection))
// 	for i, position := range trajection {
// 		index, err := PositionToIndex(position, grid.InitialPosition, grid.Width, grid.Height, grid.Scale)
// 		if err != nil {
// 			return nil, err
// 		}
// 		result[i] = index
// 	}
// 	return result, nil
// }

// func CalculateTrajectionToGrid(velocity, degrees, height float64, seed int, grid model.Grid) ([]int, error) {
// 	trajection, err := CalculateTrajection(velocity, degrees, height, seed)
// 	if err != nil {
// 		return nil, err
// 	}
// 	result := make([]int, len(trajection))
// 	for i, position := range trajection {
// 		index, err := PositionToIndex(position, grid.InitialPosition, grid.Width, grid.Height, grid.Scale)
// 		if err != nil {
// 			return nil, err
// 		}
// 		result[i] = index
// 	}
// 	return result, nil
// }
