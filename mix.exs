defmodule Mix.Tasks.Edoc do
    @shortdoc "Make docs using edoc on erlang.mk"

    def run(_) do
    {result, _error_code} = System.cmd("make", ["docs"], stderr_to_stdout: true)
    Mix.shell.info result
    :ok
    end
end

defmodule Tinymt.Mixfile do
  use Mix.Project

  def project do
    [app: :tinymt,
     version: "0.3.1",
     description: description,
     package: package,
     aliases: [docs: ["edoc"]],
     deps: deps]
  end

  def application do
    [applications: [:kernel, :stdlib, :logger]]
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
        "doc/overview.edoc",
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
        "mix.exs"
        ],
     maintainers: [
        "Kenji Rikitake"
        ],
     licenses: ["simplified BSD"],
     links: %{"GitHub" => "https://github.com/jj1bdx/tinymt-erlang/",
     "Docs" => "http://hexdocs.pm/tinymt"}
     ]
  end
end
