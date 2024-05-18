namespace Nugen.Data

type TrimmedFrameInfo =
  { Width: int
    Height: int
    AxisX: int
    AxisY: int
  }
  static member Empty = { Width = 0; Height = 0; AxisX = 0; AxisY = 0 }