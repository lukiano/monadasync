package monadasync

import java.util.concurrent.Executor

object SameThreadExecutor extends Executor {
  def execute(command: Runnable): Unit = command.run()
}
