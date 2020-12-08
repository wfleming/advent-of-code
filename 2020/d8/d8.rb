#!/usr/bin/env ruby

require_relative "machine.rb"

program = Machine::Program.parse(File.read(ARGV[0]))

def run_until_looped(vm)
  seen_instructions = []

  while !seen_instructions.include?(vm.ip)
    seen_instructions << vm.ip
    vm.step
  end
end

module P2
  InfiniteLoop = Class.new(StandardError)

  def self.run_until_exited(vm)
    seen_instructions = []

    until vm.exited?
      raise InfiniteLoop, "the vm looped" if seen_instructions.include?(vm.ip)

      seen_instructions << vm.ip
      vm.step
    end
  end

  def self.patchable?(program, patch_ip)
    %i[nop jmp].include?(program[patch_ip][0])
  end

  def self.patch_program(program, patch_ip)
    p2 = program.clone

    case p2[patch_ip][0]
    when :nop
      p2[patch_ip][0] = :jmp
    when :jmp
      p2[patch_ip][0] = :nop
    else
      raise ArgumentError, "should not be patching this instruction '#{p2[patch_ip]}' at #{patch_ip}"
    end

    p2
  end

  def self.debug_loop(program)
    # start patching from end - corrupted instr is more likely to be near the end, I think
    (0..(program.count - 1)).reverse_each do |patch_ip|
      next unless patchable?(program, patch_ip)

      begin
        patched_program = patch_program(program, patch_ip)
        vm = Machine::VM.new(patched_program)
        run_until_exited(vm)
        return [patch_ip, vm]
      rescue InfiniteLoop
        next
      end
    end

    raise RuntimeError, "debug_loop did not find a patch that completed successfully"
  end
end

# p1
p1_vm = Machine::VM.new(program.clone)
run_until_looped(p1_vm)
puts "p1: vm acc register is #{p1_vm.reg_acc}"

# p2
patched_ip, p2_vm = P2.debug_loop(program.clone)
puts "p2: patched instruction at #{patched_ip} to avoid loop. vm acc register is #{p2_vm.reg_acc}"
