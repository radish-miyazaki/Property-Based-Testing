defmodule PbtTest do
  use ExUnit.Case
  use PropCheck

  property "always work" do
    forall type <- term() do
      boolean(type)
    end
  end

  def boolean(_) do
    true
  end
end
