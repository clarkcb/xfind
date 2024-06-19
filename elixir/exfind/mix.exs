defmodule ExFind.MixProject do
  use Mix.Project

  def project do
    [
      app: :exfind,
      version: "0.1.0",
      elixir: "~> 1.17",
      escript: [main_module: ExFind.Main, path: "./bin/exfind"],
      # start_permanent: Mix.env() == :prod,
      start_permanent: true,
      deps: deps(),
      name: "exfind"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:json, "~> 1.4"},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
    ]
  end
end
