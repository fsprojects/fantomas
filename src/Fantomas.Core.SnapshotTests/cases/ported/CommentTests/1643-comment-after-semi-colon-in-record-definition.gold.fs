type T =
    { id: int
      value: RT.Dval
      retries: int
      canvasID: CanvasID
      canvasName: string
      module_: string
      name: string
      modifier: string
      // Delay in ms since it entered the queue
      delay: float }
