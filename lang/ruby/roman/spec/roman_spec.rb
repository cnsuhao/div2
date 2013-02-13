require "roman.rb"

describe "Roman.<numeral>" do
    it "returns value for single numerals" do
       Roman.I.should eq 1
       Roman.V.should eq 5
       Roman.X.should eq 10
       Roman.L.should eq 50
       Roman.C.should eq 100
       Roman.D.should eq 500
       Roman.M.should eq 1000
    end

   it "handles multiple numerals" do
       Roman.II.should eq 2
       Roman.XXVI.should eq 26
       Roman.MDCCCCLXXXXVIIII.should eq 1999
   end

   it "handles lower case" do
       Roman.xx.should eq 20
       Roman.vi.should eq 6
   end

   it "can deal with the subtractive rule" do
       Roman.IV.should eq 4
       Roman.IX.should eq 9
       Roman.XLIV.should eq 44
       Roman.MIM.should eq 1999
   end
   
   it "raises errors for invalid numerals" do
       expect { Roman.ZEBRA }.to raise_error RomanNumeralError
   end

   it "can be extended with new symbols" do
       Roman::SYMBOLS["Q"] = 5000
       Roman::SYMBOLS["U"] = 10000

       Roman.QIX.should eq 5009
       Roman.QUIL.should eq 5049
       Roman.MUD.should eq 9500
   end
end
