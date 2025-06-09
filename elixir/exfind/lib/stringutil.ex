defmodule ExFind.StringUtil do
  @moduledoc """
  Documentation for `ExFind.StringUtil`.
  """

  def atomize(string) do
    String.replace(string, "-", "_") |> String.to_atom()
  end

  def to_datetime(date_string) do
    # For now we are only going to accept ISO 8601 date or datetime strings,
    # and if the date string does not include a time, we will assume midnight UTC.
    # If the datetime string is not valid, we will just return nil.
    midnight_utc = "T00:00:00Z"
    dt_string = if Regex.match?(~r/^\d{4}\-\d{2}\-\d{2}$/, date_string) do
                  date_string <> midnight_utc
                else
                  date_string
                end
    case DateTime.from_iso8601(dt_string) do
      {:ok, dt, _offset} -> dt
      {:error, _} -> nil
    end
  end

  def to_timestamp(date_string) do
    dt = to_datetime(date_string)
    if dt != nil do
      DateTime.to_unix(dt)
    else
      0
    end
  end

  def any_matches_any_pattern(strings, patterns) do
    Enum.any?(strings, fn s -> Enum.any?(patterns, fn p -> Regex.match?(p, s) end) end)
  end

  def last_index_of(string, pattern) do
    rev_idx =
      string
      |> String.reverse()
      |> :binary.match(pattern)

    case rev_idx do
      {idx, _len} -> String.length(string) - idx - 1
      :nomatch -> -1
    end
  end

end
