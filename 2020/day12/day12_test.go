package day12

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestRotation(t *testing.T) {
	f := NewFerry()
	require.Equal(t, "E", f.Direction)
	require.NoError(t, f.MoveNormal("R90"))
	require.Equal(t, "S", f.Direction)
	require.NoError(t, f.MoveNormal("L90"))
	require.Equal(t, "E", f.Direction)

	require.NoError(t, f.MoveNormal("R180"))
	require.Equal(t, "W", f.Direction)
	require.NoError(t, f.MoveNormal("L180"))
	require.Equal(t, "E", f.Direction)

	require.NoError(t, f.MoveNormal("R270"))
	require.Equal(t, "N", f.Direction)
	require.NoError(t, f.MoveNormal("L270"))
	require.Equal(t, "E", f.Direction)
}

func TestMovement(t *testing.T) {
	commands := []string{
		"F10",
		"N3",
		"F7",
		"R90",
		"F11",
	}

	f := NewFerry()

	for _, c := range commands {
		require.NoError(t, f.MoveNormal(c))
	}

	require.Equal(t, 25, f.Position.DistanceFromOrigin())
}

func TestWaypointMovement(t *testing.T) {
	commands := []string{
		"F10",
		"N3",
		"F7",
		"R90",
		"F11",
	}

	f := NewFerry()

	for _, c := range commands {
		require.NoError(t, f.MoveWaypointed(c))
	}

	require.Equal(t, 286, f.Position.DistanceFromOrigin())
}

func TestRotatePoint(t *testing.T) {
	ship := Point{X: 10, Y: 4}
	rotated := ship.Rotate(90)
	require.Equal(t, 4, rotated.X)
	require.Equal(t, -10, rotated.Y)
	require.Equal(t, 10, ship.X)
	require.Equal(t, 4, ship.Y)
}
