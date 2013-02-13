#!/usr/bin/env ruby
# desc_tree.rb
# 
# A simple file to scrape all the READMEs


require "find"

$target = "README.md"
$list_re = /^ \* \*{2}([^*:]+):?\*{2}\s(.*)\s*$/
$dash = "\\dotfill"
$repo = "http://github.com/alecgoebel/div2"

class Node
    include Enumerable

    attr_reader :desc, :path, :children

    def initialize(path, desc=nil)
        @path = path
        @desc = desc.strip unless desc.nil? or desc.strip.empty?
        @children = []

        self.scrape if self.directory? and File.exists?(self.filepath)
    end

    def each (&block) 
        block.call(self)
        @children.each { |child| child.each &block }
    end

    def render(render, stepdown=nil, stepup=nil, &block)
        yield render.call(self)
        if self.directory? and not @children.empty?
            yield stepdown.call(self)
            @children.each { |child| child.render(render, stepdown, stepup, &block) }
            yield stepup.call(self)
        end
    end

    def to_s; "Node (#{self.name}): #{@desc}" end
    def name; File.basename @path end

    def directory?; File.directory? @path end
    def filepath; File.join(@path, $target) end

    def scrape
        open(self.filepath).select { |line|
            $list_re.match(line)
        }.each { |line|
            line, path, desc = $list_re.match(line).to_a
            @children << Node.new(File.join(@path,path), desc)
        }
    end
end

tree = Node.new(ARGV[0])

# render to latex
outp = File.open(ARGV[1], 'w') do |f|
    begin_list = lambda {|n| '\begin{description}'}
    end_list = lambda {|n|  '\end{description}'}
    list_item = lambda do |n| 
        path = n.path.sub(ARGV[0]+'/', "")
        path.sub!(/clojure\/sandbox/, "clojure/sandbox/src/sandbox")

        url = File.join $repo, 'blob', 'master', path
        #print path
        #print "\t\tMISSING" unless n.directory? or File.exists?(ARGV[0] + '/' + path)
        #puts ""
        "\\item \\textbf{\\href{#{url}}{#{n.name}}} \\dotfill #{n.desc}"

    end

    f.write begin_list.call(tree) + "\n"
    tree.children.each do |child|
        child.render(list_item, begin_list, end_list) do |s|
            s.gsub!(/_/, '\\_')
            f.write(s + "\n")
        end
    end
    f.write end_list.call(tree) + "\n"
end
