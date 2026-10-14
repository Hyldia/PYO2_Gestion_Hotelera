-- Valores y calculos relacionados con el impuesto de ventas (IVA)
module Utils.Impuestos where
    {-
    * Tasa del impuesto de ventas (IVA) del 13%.
    * Se define una sola vez para que facturación y estadisticas usen siempre el mismo valor.
    -}
    tasaIVA :: Double
    tasaIVA = 0.13