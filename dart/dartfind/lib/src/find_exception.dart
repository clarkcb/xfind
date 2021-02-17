class FindException implements Exception {
  final String message;

  const FindException(this.message);

  @override
  String toString() => message;
}
