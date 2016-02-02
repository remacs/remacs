class A
 def a()
  super(" do ")
 end
 def b()
 end
end

module A
  class B
    ABC = 4
    Def_ = 'blah'
    Xyzzy =10

    def foo!
    end

    def self._bar?(abc)
    end

    class << self
      def qux=(tee)
      end
    end
    def X
      attr_reader :foo
      attr_reader :read1, :read2; attr_writer :write1, :write2
      attr_writer :bar
      attr_accessor :tee
      alias_method :qux, :tee
    end
  end
end

A::Constant = 5

# def foo_in_comment
# end
