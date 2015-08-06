# MonadAsync

Because Task is just an EitherT[ Future, Throwable, ? ].

Because Task's handy operations (handle, timed, retry) are lost when you have a WriterT[ Task, ? ]

## How-To

Please refer to existing tests. 

