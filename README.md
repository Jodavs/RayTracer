This project renders reference scenes using the ray tracer API
`Tracer.API`, which is found in [`API.fsi`](API.fsi). The project file
is located at `TracerTestSuite/TracerTestSuite.fsproj`. It expects a
valid implementation of the module `Tracer.API`. A dummy
implementation of the API is given in [`API.fs`](API.fs).

You can link the test suite to your implementation in two different
ways:

1. The implementation of `Tracer.API` is compiled to a DLL file, which
   is then added as a reference to the `TracerTestSuite.fsproj`
   project file.
2. The project `TracerTestSuite.fsproj` is added to the solution file
   that contains the implementation of `Tracer.API`. The project that
   implements `Tracer.API` is then added as a reference to the
   `TracerTestSuite.fsproj` project.
   
The first method is very easy to set up, but second method should be
preferred as you can run the test suite and debug your implementation
within the same solution. For the final hand-in of the project we
expect your code to be in the second form, so that we can compile and
run your implementation.

Once your implementation has been added as a reference to the
`TracerTestSuite.fsproj` project, the project can be compiled and
executed. By default the rendered images are stored as files in the
`result` subdirectory. The corresponding results of our implementation
are included in the [`reference`](reference) subdirectory.
