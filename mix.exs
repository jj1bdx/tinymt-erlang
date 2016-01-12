defmodule Tinymt.Mixfile do
  use Mix.Project

  def project do
    [app: :tinymt,
     version: "0.3.0",
     description: description,
     package: package,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    []
  end

  defp description do
    """
    Tiny Mersenne Twister (TinyMT) for Erlang
    """
  end

  defp package do
    [files: [
        ".gitignore",
        ".travis.yml",
        "CONTRIBUTING.md",
        "LICENSE",
        "Makefile",
        "Makefile.tinymt",
        "README.md",
        "erlang.mk",
        "rng-parameters",
        "src",
        "test",
        "test-scripts",
        "tinymt-erlang.tsv",
        "mix.exs"
        ],
     maintainers: [
        "Kenji Rikitake"
        ],
     licenses: ["simplified BSD"],
     links: %{"GitHub" => "https://github.com/jj1bdx/tinymt-erlang/"}
     ]
  end
end
